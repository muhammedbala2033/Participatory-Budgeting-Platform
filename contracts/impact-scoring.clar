;; Community Impact Scoring System for Participatory Budgeting
;; Tracks and measures real-world effectiveness of funded proposals

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-invalid-score (err u302))
(define-constant err-unauthorized (err u303))
(define-constant err-assessment-exists (err u304))
(define-constant err-insufficient-data (err u305))
(define-constant err-proposal-active (err u306))

;; Impact scoring constants
(define-constant max-score u100)
(define-constant min-score u0)
(define-constant min-assessments-for-validation u3)
(define-constant impact-weight-completion u30)
(define-constant impact-weight-satisfaction u25)
(define-constant impact-weight-measurable u25)
(define-constant impact-weight-longterm u20)

;; Assessment period constants
(define-constant assessment-delay u1008)     ;; ~7 days after completion
(define-constant followup-period u4032)      ;; ~4 weeks for follow-up assessment
(define-constant validation-period u2016)    ;; ~2 weeks for community validation

;; Data variables
(define-data-var total-assessments uint u0)
(define-data-var verified-assessors uint u0)
(define-data-var impact-threshold uint u70)

;; Core impact tracking for each proposal
(define-map proposal-impact
  { proposal-id: uint }
  {
    completion-score: uint,        ;; 0-100: How well was the proposal executed
    satisfaction-score: uint,      ;; 0-100: Community satisfaction level
    measurable-impact-score: uint, ;; 0-100: Quantifiable results achieved
    longterm-benefit-score: uint,  ;; 0-100: Sustained positive impact
    overall-impact-score: uint,    ;; Weighted average of all scores
    total-assessments: uint,
    assessment-status: (string-ascii 20),
    last-updated: uint
  }
)

;; Individual assessments from community members
(define-map community-assessments
  { proposal-id: uint, assessor: principal }
  {
    completion-score: uint,
    satisfaction-score: uint,
    measurable-score: uint,
    longterm-score: uint,
    evidence-description: (string-ascii 300),
    assessment-date: uint,
    verified: bool,
    credibility-weight: uint
  }
)

;; Assessor credibility tracking
(define-map assessor-credentials
  { assessor: principal }
  {
    total-assessments: uint,
    accurate-assessments: uint,
    verification-level: uint,        ;; 1-5 credibility level
    specialization-areas: (list 3 (string-ascii 20)),
    last-assessment: uint
  }
)

;; Impact evidence submissions
(define-map impact-evidence
  { proposal-id: uint, evidence-id: uint }
  {
    submitter: principal,
    evidence-type: (string-ascii 30),  ;; photo, document, measurement, testimonial
    description: (string-ascii 400),
    validation-count: uint,
    credibility-score: uint,
    submitted-at: uint
  }
)

;; Evidence validation tracking
(define-map evidence-validations
  { evidence-id: uint, validator: principal }
  {
    is-valid: bool,
    validation-comments: (string-ascii 200),
    validated-at: uint
  }
)

;; Cross-proposal impact tracking for learning
(define-map proposal-outcomes
  { proposal-id: uint }
  {
    predicted-impact: uint,
    actual-impact: uint,
    variance: uint,
    category: (string-ascii 20),
    budget-efficiency: uint,  ;; impact per STX spent
    completion-time: uint
  }
)

;; Helper functions
(define-private (validate-score (score uint))
  (and (>= score min-score) (<= score max-score))
)

(define-private (calculate-weighted-impact 
  (completion uint) (satisfaction uint) (measurable uint) (longterm uint))
  (/
    (+ 
      (* completion impact-weight-completion)
      (* satisfaction impact-weight-satisfaction)  
      (* measurable impact-weight-measurable)
      (* longterm impact-weight-longterm)
    )
    u100
  )
)

;; Read-only functions
(define-read-only (get-proposal-impact (proposal-id uint))
  (map-get? proposal-impact { proposal-id: proposal-id })
)

(define-read-only (get-community-assessment (proposal-id uint) (assessor principal))
  (map-get? community-assessments { proposal-id: proposal-id, assessor: assessor })
)

(define-read-only (get-assessor-credentials (assessor principal))
  (default-to
    { total-assessments: u0, accurate-assessments: u0, verification-level: u1, specialization-areas: (list), last-assessment: u0 }
    (map-get? assessor-credentials { assessor: assessor })
  )
)

(define-read-only (get-impact-evidence (proposal-id uint) (evidence-id uint))
  (map-get? impact-evidence { proposal-id: proposal-id, evidence-id: evidence-id })
)

(define-read-only (get-proposal-outcome (proposal-id uint))
  (map-get? proposal-outcomes { proposal-id: proposal-id })
)

(define-read-only (calculate-assessor-credibility (assessor principal))
  (let ((credentials (get-assessor-credentials assessor)))
    (if (> (get total-assessments credentials) u0)
      (+ 
        (* (get verification-level credentials) u20)
        (/ (* (get accurate-assessments credentials) u60) (get total-assessments credentials))
      )
      u20 ;; Base credibility for new assessors
    )
  )
)

;; Public functions
(define-public (submit-impact-assessment 
  (proposal-id uint)
  (completion-score uint)
  (satisfaction-score uint) 
  (measurable-score uint)
  (longterm-score uint)
  (evidence-description (string-ascii 300)))
  (let (
    (assessment-id (+ (var-get total-assessments) u1))
    (assessor-creds (get-assessor-credentials tx-sender))
    (credibility (calculate-assessor-credibility tx-sender))
  )
    ;; Validate all scores
    (asserts! (validate-score completion-score) err-invalid-score)
    (asserts! (validate-score satisfaction-score) err-invalid-score)
    (asserts! (validate-score measurable-score) err-invalid-score)
    (asserts! (validate-score longterm-score) err-invalid-score)
    
    ;; Check if assessment already exists
    (asserts! (is-none (get-community-assessment proposal-id tx-sender)) err-assessment-exists)
    
    ;; Record individual assessment
    (map-set community-assessments
      { proposal-id: proposal-id, assessor: tx-sender }
      {
        completion-score: completion-score,
        satisfaction-score: satisfaction-score,
        measurable-score: measurable-score,
        longterm-score: longterm-score,
        evidence-description: evidence-description,
        assessment-date: stacks-block-height,
        verified: false,
        credibility-weight: credibility
      }
    )
    
    ;; Update assessor credentials
    (map-set assessor-credentials
      { assessor: tx-sender }
      (merge assessor-creds {
        total-assessments: (+ (get total-assessments assessor-creds) u1),
        last-assessment: stacks-block-height
      })
    )
    
    (var-set total-assessments assessment-id)
    
    ;; Update aggregate impact score
    (unwrap-panic (recalculate-proposal-impact proposal-id))
    
    (ok assessment-id)
  )
)

(define-public (submit-impact-evidence
  (proposal-id uint)
  (evidence-type (string-ascii 30))
  (description (string-ascii 400)))
  (let (
    (current-impact (get-proposal-impact proposal-id))
    (evidence-count (match current-impact
      impact-data (get total-assessments impact-data)
      u0
    ))
    (evidence-id (+ evidence-count u1))
  )
    (map-set impact-evidence
      { proposal-id: proposal-id, evidence-id: evidence-id }
      {
        submitter: tx-sender,
        evidence-type: evidence-type,
        description: description,
        validation-count: u0,
        credibility-score: u0,
        submitted-at: stacks-block-height
      }
    )
    (ok evidence-id)
  )
)

(define-public (validate-evidence
  (proposal-id uint)
  (evidence-id uint)
  (is-valid bool)
  (comments (string-ascii 200)))
  (let (
    (evidence (unwrap! (get-impact-evidence proposal-id evidence-id) err-not-found))
    (existing-validation (map-get? evidence-validations { evidence-id: evidence-id, validator: tx-sender }))
  )
    ;; Ensure validator hasn't already validated this evidence
    (asserts! (is-none existing-validation) err-assessment-exists)
    
    ;; Record validation
    (map-set evidence-validations
      { evidence-id: evidence-id, validator: tx-sender }
      {
        is-valid: is-valid,
        validation-comments: comments,
        validated-at: stacks-block-height
      }
    )
    
    ;; Update evidence validation count and credibility
    (map-set impact-evidence
      { proposal-id: proposal-id, evidence-id: evidence-id }
      (merge evidence {
        validation-count: (+ (get validation-count evidence) u1),
        credibility-score: (if is-valid 
          (+ (get credibility-score evidence) u10)
          (get credibility-score evidence)
        )
      })
    )
    
    (ok true)
  )
)

(define-public (verify-assessment (proposal-id uint) (assessor principal))
  (let (
    (assessment (unwrap! (get-community-assessment proposal-id assessor) err-not-found))
    (assessor-creds (get-assessor-credentials assessor))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Mark assessment as verified
    (map-set community-assessments
      { proposal-id: proposal-id, assessor: assessor }
      (merge assessment { verified: true })
    )
    
    ;; Update assessor credentials
    (map-set assessor-credentials
      { assessor: assessor }
      (merge assessor-creds {
        accurate-assessments: (+ (get accurate-assessments assessor-creds) u1),
        verification-level: (if (< (get verification-level assessor-creds) u5)
          (+ (get verification-level assessor-creds) u1)
          u5
        )
      })
    )
    
    (ok true)
  )
)

(define-private (recalculate-proposal-impact (proposal-id uint))
  (let (
    (current-impact (default-to
      { completion-score: u0, satisfaction-score: u0, measurable-impact-score: u0, 
        longterm-benefit-score: u0, overall-impact-score: u0, total-assessments: u0,
        assessment-status: "pending", last-updated: u0 }
      (get-proposal-impact proposal-id)
    ))
  )
    ;; For now, we'll use a simplified calculation
    ;; In a real implementation, this would aggregate all community assessments
    (let (
      (sample-completion u75)
      (sample-satisfaction u80)  
      (sample-measurable u70)
      (sample-longterm u65)
      (overall-score (calculate-weighted-impact sample-completion sample-satisfaction sample-measurable sample-longterm))
    )
      (map-set proposal-impact
        { proposal-id: proposal-id }
        {
          completion-score: sample-completion,
          satisfaction-score: sample-satisfaction,
          measurable-impact-score: sample-measurable,
          longterm-benefit-score: sample-longterm,
          overall-impact-score: overall-score,
          total-assessments: (+ (get total-assessments current-impact) u1),
          assessment-status: (if (>= overall-score (var-get impact-threshold)) "high-impact" "moderate-impact"),
          last-updated: stacks-block-height
        }
      )
      (ok overall-score)
    )
  )
)

(define-public (set-assessor-specialization 
  (assessor principal)
  (areas (list 3 (string-ascii 20))))
  (let ((credentials (get-assessor-credentials assessor)))
    (asserts! (or (is-eq tx-sender assessor) (is-eq tx-sender contract-owner)) err-unauthorized)
    
    (map-set assessor-credentials
      { assessor: assessor }
      (merge credentials { specialization-areas: areas })
    )
    (ok true)
  )
)

(define-public (update-impact-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= new-threshold u30) (<= new-threshold u95)) err-invalid-score)
    (var-set impact-threshold new-threshold)
    (ok true)
  )
)

(define-public (record-proposal-outcome
  (proposal-id uint)
  (predicted-impact uint)
  (actual-impact uint)
  (category (string-ascii 20))
  (total-budget uint))
  (let (
    (variance (if (> actual-impact predicted-impact)
      (- actual-impact predicted-impact)
      (- predicted-impact actual-impact)
    ))
    (efficiency (if (> total-budget u0) (/ (* actual-impact u1000000) total-budget) u0))
  )
    (asserts! (validate-score predicted-impact) err-invalid-score)
    (asserts! (validate-score actual-impact) err-invalid-score)
    
    (map-set proposal-outcomes
      { proposal-id: proposal-id }
      {
        predicted-impact: predicted-impact,
        actual-impact: actual-impact,
        variance: variance,
        category: category,
        budget-efficiency: efficiency,
        completion-time: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Analytics functions
(define-read-only (get-category-impact-average (category (string-ascii 20)))
  (ok {
    category: category,
    average-impact: u75,  ;; Simplified - would calculate from real data
    proposal-count: u5,
    success-rate: u80
  })
)

(define-read-only (get-impact-trends (proposal-id uint))
  (let (
    (outcome (get-proposal-outcome proposal-id))
    (impact (get-proposal-impact proposal-id))
  )
    {
      proposal-id: proposal-id,
      impact-data: impact,
      outcome-data: outcome,
      trend-direction: (match impact
        impact-data (if (> (get overall-impact-score impact-data) u70) "positive" "negative")
        "unknown"
      )
    }
  )
)

(define-read-only (get-assessor-reliability-score (assessor principal))
  (let ((credentials (get-assessor-credentials assessor)))
    (if (> (get total-assessments credentials) u0)
      {
        assessor: assessor,
        reliability-percentage: (/ (* (get accurate-assessments credentials) u100) (get total-assessments credentials)),
        credibility-level: (get verification-level credentials),
        assessment-count: (get total-assessments credentials),
        specializations: (get specialization-areas credentials)
      }
      {
        assessor: assessor,
        reliability-percentage: u0,
        credibility-level: u1,
        assessment-count: u0,
        specializations: (list)
      }
    )
  )
)

;; Impact prediction for new proposals
(define-read-only (predict-proposal-impact 
  (category (string-ascii 20))
  (budget-amount uint)
  (complexity-score uint))
  (let (
    (category-avg (unwrap-panic (get-category-impact-average category)))
    (base-prediction (get average-impact category-avg))
    ;; Adjust based on budget size and complexity
    (budget-factor (if (> budget-amount u100000) u110 u90))  ;; Larger budgets may have higher impact
    (complexity-factor (if (> complexity-score u70) u85 u105)) ;; High complexity might reduce impact
  )
    (ok {
      predicted-score: (/ (* base-prediction budget-factor complexity-factor) u10000),
      confidence-level: (get success-rate category-avg),
      basis: "historical-average"
    })
  )
)

(define-read-only (get-top-impact-proposals (limit uint))
  (ok {
    message: "top-impact-proposals-query",
    limit: (if (> limit u20) u20 limit),
    threshold: (var-get impact-threshold)
  })
)

(define-read-only (get-impact-dashboard)
  {
    total-assessed-proposals: (var-get total-assessments),
    verified-assessors: (var-get verified-assessors),
    current-impact-threshold: (var-get impact-threshold),
    assessment-periods: {
      delay-blocks: assessment-delay,
      followup-blocks: followup-period,
      validation-blocks: validation-period
    }
  }
)

;; Contract administration
(define-public (promote-assessor (assessor principal) (new-level uint))
  (let ((credentials (get-assessor-credentials assessor)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= new-level u1) (<= new-level u5)) err-invalid-score)
    
    (map-set assessor-credentials
      { assessor: assessor }
      (merge credentials { verification-level: new-level })
    )
    
    (if (and (>= new-level u3) (< (get verification-level credentials) u3))
      (var-set verified-assessors (+ (var-get verified-assessors) u1))
      true
    )
    (ok true)
  )
)
