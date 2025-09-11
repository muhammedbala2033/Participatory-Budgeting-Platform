(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-EXISTS (err u101))
(define-constant ERR-NO-PROPOSAL (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))

(define-constant ERR-ESCROW-NOT-FOUND (err u106))
(define-constant ERR-ESCROW-ALREADY-EXISTS (err u107))
(define-constant ERR-INSUFFICIENT-ESCROW-FUNDS (err u108))
(define-constant ERR-INVALID-STAGE (err u109))
(define-constant ERR-STAGE-NOT-READY (err u110))
(define-constant ERR-DISPUTE-ACTIVE (err u111))
(define-constant ERR-RATING-ALREADY-GIVEN (err u112))
(define-constant ERR-INVALID-RATING (err u113))

(define-map escrow-accounts
    { proposal-id: uint }
    {
        total-amount: uint,
        released-amount: uint,
        stage-count: uint,
        current-stage: uint,
        contractor: principal,
        dispute-active: bool,
        created-at: uint
    }
)

(define-map escrow-stages
    { proposal-id: uint, stage-id: uint }
    {
        amount: uint,
        description: (string-ascii 200),
        completed: bool,
        release-approved: bool,
        completion-time: uint,
        approver: (optional principal)
    }
)

(define-map escrow-funds
    { proposal-id: uint }
    { balance: uint }
)

(define-map stage-approvals
    { proposal-id: uint, stage-id: uint, approver: principal }
    { approved: bool, timestamp: uint }
)

(define-map contractor-ratings
    { proposal-id: uint, rater: principal }
    { 
        rating: uint,
        comment: (string-ascii 100),
        given-at: uint
    }
)

(define-map contractor-performance
    { contractor: principal }
    {
        total-projects: uint,
        completed-projects: uint,
        average-rating: uint,
        total-ratings: uint,
        on-time-completions: uint
    }
)

(define-map dispute-cases
    { proposal-id: uint }
    {
        raised-by: principal,
        reason: (string-ascii 300),
        resolved: bool,
        resolution: (string-ascii 200),
        resolver: (optional principal),
        created-at: uint
    }
)

(define-data-var min-approvers uint u3)
(define-data-var dispute-resolution-period uint u1008)
(define-data-var auto-release-delay uint u2016)

(define-data-var total-budget uint u1000000)
(define-data-var proposal-count uint u0)
(define-data-var voting-period uint u144)
(define-data-var min-proposal-amount uint u1000)

(define-map proposals 
    { proposal-id: uint }
    {
        creator: principal,
        title: (string-ascii 50),
        description: (string-ascii 500),
        amount: uint,
        votes: uint,
        status: (string-ascii 20),
        created-at: uint
    }
)

(define-map votes
    { voter: principal, proposal-id: uint }
    { voted: bool }
)

(define-public (create-proposal (title (string-ascii 50)) (description (string-ascii 500)) (amount uint))
    (let
        (
            (new-id (+ (var-get proposal-count) u1))
        )
        (asserts! (>= amount (var-get min-proposal-amount)) ERR-INSUFFICIENT-FUNDS)
        (asserts! (<= amount (var-get total-budget)) ERR-INSUFFICIENT-FUNDS)
        (asserts! (map-insert proposals
            { proposal-id: new-id }
            {
                creator: tx-sender,
                title: title,
                description: description,
                amount: amount,
                votes: u0,
                status: "active",
                created-at: stacks-block-height
            }
        ) ERR-PROPOSAL-EXISTS)
        (var-set proposal-count new-id)
        (ok new-id)
    )
)

(define-public (vote (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-PROPOSAL))
            (vote-status (default-to { voted: false } (map-get? votes { voter: tx-sender, proposal-id: proposal-id })))
        )
        (asserts! (< (- stacks-block-height (get created-at proposal)) (var-get voting-period)) ERR-VOTING-CLOSED)
        (asserts! (not (get voted vote-status)) ERR-ALREADY-VOTED)
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { votes: (+ (get votes proposal) u1) })
        )
        (map-set votes
            { voter: tx-sender, proposal-id: proposal-id }
            { voted: true }
        )
        (ok true)
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-PROPOSAL))
        )
        (asserts! (>= (- stacks-block-height (get created-at proposal)) (var-get voting-period)) ERR-VOTING-CLOSED)
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { status: "completed" })
        )
        (ok true)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (ok (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-PROPOSAL))
)

(define-read-only (get-vote-status (proposal-id uint) (voter principal))
    (ok (default-to { voted: false } (map-get? votes { voter: voter, proposal-id: proposal-id })))
)

(define-read-only (get-total-proposals)
    (ok (var-get proposal-count))
)

(define-read-only (get-voting-period)
    (ok (var-get voting-period))
)

(define-read-only (get-total-budget)
    (ok (var-get total-budget))
)


(define-map category-budgets
    { category: (string-ascii 20) }
    { 
        cap: uint,
        used: uint
    }
)

(define-map proposal-categories
    { proposal-id: uint }
    { category: (string-ascii 20) }
)

(define-public (set-category-budget (category (string-ascii 20)) (cap uint))
    (begin
        (map-set category-budgets
            { category: category }
            { cap: cap, used: u0 }
        )
        (ok true)
    )
)

(define-public (create-proposal-with-category 
    (title (string-ascii 50)) 
    (description (string-ascii 500)) 
    (amount uint)
    (category (string-ascii 20)))
    (let
        (
            (new-id (+ (var-get proposal-count) u1))
            (category-budget (unwrap! (map-get? category-budgets { category: category }) ERR-NO-PROPOSAL))
        )
        (asserts! (>= amount (var-get min-proposal-amount)) ERR-INSUFFICIENT-FUNDS)
        (asserts! (<= (+ amount (get used category-budget)) (get cap category-budget)) ERR-INSUFFICIENT-FUNDS)
        (map-set category-budgets
            { category: category }
            { cap: (get cap category-budget), used: (+ amount (get used category-budget)) }
        )
        (map-set proposal-categories { proposal-id: new-id } { category: category })
        (create-proposal title description amount)
    )
)


(define-map proposal-milestones
    { proposal-id: uint, milestone-id: uint }
    {
        description: (string-ascii 100),
        amount: uint,
        completed: bool
    }
)

(define-map milestone-counts
    { proposal-id: uint }
    { count: uint }
)

(define-public (add-milestone 
    (proposal-id uint) 
    (description (string-ascii 100))
    (amount uint))
    (let
        (
            (milestone-count (default-to { count: u0 } (map-get? milestone-counts { proposal-id: proposal-id })))
            (new-milestone-id (+ (get count milestone-count) u1))
        )
        (map-set proposal-milestones
            { proposal-id: proposal-id, milestone-id: new-milestone-id }
            { description: description, amount: amount, completed: false }
        )
        (map-set milestone-counts
            { proposal-id: proposal-id }
            { count: new-milestone-id }
        )
        (ok new-milestone-id)
    )
)

(define-public (complete-milestone (proposal-id uint) (milestone-id uint))
    (let
        (
            (milestone (unwrap! (map-get? proposal-milestones { proposal-id: proposal-id, milestone-id: milestone-id }) ERR-NO-PROPOSAL))
        )
        (map-set proposal-milestones
            { proposal-id: proposal-id, milestone-id: milestone-id }
            (merge milestone { completed: true })
        )
        (ok true)
    )
)


(define-map delegations
    { delegator: principal }
    { delegate: principal, active: bool }
)

(define-map delegate-vote-power
    { delegate: principal }
    { power: uint }
)

(define-map proposal-direct-votes
    { voter: principal, proposal-id: uint }
    { direct-vote: bool }
)

(define-public (delegate-vote (delegate principal))
    (let
        (
            (current-delegation (map-get? delegations { delegator: tx-sender }))
            (current-power (default-to { power: u0 } (map-get? delegate-vote-power { delegate: delegate })))
        )
        (match current-delegation
            existing-delegation
            (if (get active existing-delegation)
                (let
                    (
                        (old-delegate (get delegate existing-delegation))
                        (old-power (default-to { power: u0 } (map-get? delegate-vote-power { delegate: old-delegate })))
                    )
                    (map-set delegate-vote-power
                        { delegate: old-delegate }
                        { power: (- (get power old-power) u1) }
                    )
                    (map-set delegations
                        { delegator: tx-sender }
                        { delegate: delegate, active: true }
                    )
                    (map-set delegate-vote-power
                        { delegate: delegate }
                        { power: (+ (get power current-power) u1) }
                    )
                    (ok true)
                )
                (begin
                    (map-set delegations
                        { delegator: tx-sender }
                        { delegate: delegate, active: true }
                    )
                    (map-set delegate-vote-power
                        { delegate: delegate }
                        { power: (+ (get power current-power) u1) }
                    )
                    (ok true)
                )
            )
            (begin
                (map-set delegations
                    { delegator: tx-sender }
                    { delegate: delegate, active: true }
                )
                (map-set delegate-vote-power
                    { delegate: delegate }
                    { power: (+ (get power current-power) u1) }
                )
                (ok true)
            )
        )
    )
)

(define-public (revoke-delegation)
    (let
        (
            (delegation (unwrap! (map-get? delegations { delegator: tx-sender }) ERR-NOT-AUTHORIZED))
        )
        (asserts! (get active delegation) ERR-NOT-AUTHORIZED)
        (let
            (
                (delegate (get delegate delegation))
                (current-power (default-to { power: u0 } (map-get? delegate-vote-power { delegate: delegate })))
            )
            (map-set delegations
                { delegator: tx-sender }
                { delegate: delegate, active: false }
            )
            (map-set delegate-vote-power
                { delegate: delegate }
                { power: (- (get power current-power) u1) }
            )
            (ok true)
        )
    )
)

(define-public (vote-as-delegate (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-PROPOSAL))
            (vote-status (default-to { voted: false } (map-get? votes { voter: tx-sender, proposal-id: proposal-id })))
            (delegate-power (default-to { power: u0 } (map-get? delegate-vote-power { delegate: tx-sender })))
            (total-votes (+ u1 (get power delegate-power)))
        )
        (asserts! (< (- stacks-block-height (get created-at proposal)) (var-get voting-period)) ERR-VOTING-CLOSED)
        (asserts! (not (get voted vote-status)) ERR-ALREADY-VOTED)
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { votes: (+ (get votes proposal) total-votes) })
        )
        (map-set votes
            { voter: tx-sender, proposal-id: proposal-id }
            { voted: true }
        )
        (ok total-votes)
    )
)

(define-public (vote-direct (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-PROPOSAL))
            (vote-status (default-to { voted: false } (map-get? votes { voter: tx-sender, proposal-id: proposal-id })))
            (delegation (map-get? delegations { delegator: tx-sender }))
        )
        (asserts! (< (- stacks-block-height (get created-at proposal)) (var-get voting-period)) ERR-VOTING-CLOSED)
        (asserts! (not (get voted vote-status)) ERR-ALREADY-VOTED)
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { votes: (+ (get votes proposal) u1) })
        )
        (map-set votes
            { voter: tx-sender, proposal-id: proposal-id }
            { voted: true }
        )
        (map-set proposal-direct-votes
            { voter: tx-sender, proposal-id: proposal-id }
            { direct-vote: true }
        )
        (ok true)
    )
)

(define-read-only (get-delegation (delegator principal))
    (ok (map-get? delegations { delegator: delegator }))
)

(define-read-only (get-delegate-power (delegate principal))
    (ok (default-to { power: u0 } (map-get? delegate-vote-power { delegate: delegate })))
)

(define-read-only (is-direct-vote (voter principal) (proposal-id uint))
    (ok (default-to { direct-vote: false } (map-get? proposal-direct-votes { voter: voter, proposal-id: proposal-id })))
)

(define-public (create-escrow-account 
    (proposal-id uint) 
    (contractor principal) 
    (stage-descriptions (list 10 (string-ascii 200)))
    (stage-amounts (list 10 uint)))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-PROPOSAL))
            (total-amount (get amount proposal))
            (stage-count (len stage-descriptions))
            (sum-amounts (fold + stage-amounts u0))
        )
        (asserts! (is-none (map-get? escrow-accounts { proposal-id: proposal-id })) ERR-ESCROW-ALREADY-EXISTS)
        (asserts! (is-eq sum-amounts total-amount) ERR-INSUFFICIENT-ESCROW-FUNDS)
        (asserts! (and (> stage-count u0) (<= stage-count u10)) ERR-INVALID-STAGE)
        (map-set escrow-accounts
            { proposal-id: proposal-id }
            {
                total-amount: total-amount,
                released-amount: u0,
                stage-count: stage-count,
                current-stage: u1,
                contractor: contractor,
                dispute-active: false,
                created-at: stacks-block-height
            }
        )
        (map-set escrow-funds
            { proposal-id: proposal-id }
            { balance: total-amount }
        )
        (fold create-stage-entry (zip stage-descriptions stage-amounts) { proposal-id: proposal-id, stage-id: u1 })
        (ok true)
    )
)

(define-private (create-stage-entry 
    (entry { description: (string-ascii 200), amount: uint }) 
    (acc { proposal-id: uint, stage-id: uint }))
    (let
        (
            (stage-id (get stage-id acc))
            (proposal-id (get proposal-id acc))
        )
        (map-set escrow-stages
            { proposal-id: proposal-id, stage-id: stage-id }
            {
                amount: (get amount entry),
                description: (get description entry),
                completed: false,
                release-approved: false,
                completion-time: u0,
                approver: none
            }
        )
        { proposal-id: proposal-id, stage-id: (+ stage-id u1) }
    )
)

(define-private (zip (list-a (list 10 (string-ascii 200))) (list-b (list 10 uint)))
    (let
        (
            (len-a (len list-a))
            (len-b (len list-b))
        )
        (if (is-eq len-a len-b)
            (map combine-elements list-a list-b)
            (list)
        )
    )
)

(define-private (combine-elements (desc (string-ascii 200)) (amt uint))
    { description: desc, amount: amt }
)

(define-public (complete-escrow-stage (proposal-id uint) (stage-id uint))
    (let
        (
            (escrow (unwrap! (map-get? escrow-accounts { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
            (stage (unwrap! (map-get? escrow-stages { proposal-id: proposal-id, stage-id: stage-id }) ERR-INVALID-STAGE))
        )
        (asserts! (is-eq (get contractor escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get current-stage escrow) stage-id) ERR-STAGE-NOT-READY)
        (asserts! (not (get dispute-active escrow)) ERR-DISPUTE-ACTIVE)
        (asserts! (not (get completed stage)) ERR-ALREADY-VOTED)
        (map-set escrow-stages
            { proposal-id: proposal-id, stage-id: stage-id }
            (merge stage { 
                completed: true, 
                completion-time: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-public (approve-stage-release (proposal-id uint) (stage-id uint))
    (let
        (
            (escrow (unwrap! (map-get? escrow-accounts { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
            (stage (unwrap! (map-get? escrow-stages { proposal-id: proposal-id, stage-id: stage-id }) ERR-INVALID-STAGE))
            (existing-approval (map-get? stage-approvals { proposal-id: proposal-id, stage-id: stage-id, approver: tx-sender }))
        )
        (asserts! (get completed stage) ERR-STAGE-NOT-READY)
        (asserts! (is-none existing-approval) ERR-ALREADY-VOTED)
        (asserts! (not (get dispute-active escrow)) ERR-DISPUTE-ACTIVE)
        (map-set stage-approvals
            { proposal-id: proposal-id, stage-id: stage-id, approver: tx-sender }
            { approved: true, timestamp: stacks-block-height }
        )
        (try! (check-and-release-stage-funds proposal-id stage-id))
        (ok true)
    )
)

(define-private (check-and-release-stage-funds (proposal-id uint) (stage-id uint))
    (let
        (
            (escrow (unwrap! (map-get? escrow-accounts { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
            (stage (unwrap! (map-get? escrow-stages { proposal-id: proposal-id, stage-id: stage-id }) ERR-INVALID-STAGE))
            (approval-count (get-approval-count proposal-id stage-id))
            (funds (unwrap! (map-get? escrow-funds { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
        )
        (if (>= approval-count (var-get min-approvers))
            (begin
                (map-set escrow-stages
                    { proposal-id: proposal-id, stage-id: stage-id }
                    (merge stage { 
                        release-approved: true,
                        approver: (some tx-sender)
                    })
                )
                (map-set escrow-funds
                    { proposal-id: proposal-id }
                    { balance: (- (get balance funds) (get amount stage)) }
                )
                (map-set escrow-accounts
                    { proposal-id: proposal-id }
                    (merge escrow { 
                        released-amount: (+ (get released-amount escrow) (get amount stage)),
                        current-stage: (+ (get current-stage escrow) u1)
                    })
                )
                (ok true)
            )
            (ok false)
        )
    )
)

(define-read-only (get-approval-count (proposal-id uint) (stage-id uint))
    (let
        (
            (approvers (list tx-sender))
        )
        (fold count-approvals approvers u0)
    )
)

(define-private (count-approvals (approver principal) (count uint))
    (+ count u1)
)

(define-public (raise-dispute (proposal-id uint) (reason (string-ascii 300)))
    (let
        (
            (escrow (unwrap! (map-get? escrow-accounts { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
            (existing-dispute (map-get? dispute-cases { proposal-id: proposal-id }))
        )
        (asserts! (is-none existing-dispute) ERR-DISPUTE-ACTIVE)
        (map-set dispute-cases
            { proposal-id: proposal-id }
            {
                raised-by: tx-sender,
                reason: reason,
                resolved: false,
                resolution: "",
                resolver: none,
                created-at: stacks-block-height
            }
        )
        (map-set escrow-accounts
            { proposal-id: proposal-id }
            (merge escrow { dispute-active: true })
        )
        (ok true)
    )
)

(define-public (resolve-dispute (proposal-id uint) (resolution (string-ascii 200)))
    (let
        (
            (escrow (unwrap! (map-get? escrow-accounts { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
            (dispute (unwrap! (map-get? dispute-cases { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
        )
        (asserts! (get dispute-active escrow) ERR-ESCROW-NOT-FOUND)
        (asserts! (not (get resolved dispute)) ERR-ALREADY-VOTED)
        (map-set dispute-cases
            { proposal-id: proposal-id }
            (merge dispute { 
                resolved: true,
                resolution: resolution,
                resolver: (some tx-sender)
            })
        )
        (map-set escrow-accounts
            { proposal-id: proposal-id }
            (merge escrow { dispute-active: false })
        )
        (ok true)
    )
)

(define-public (rate-contractor (proposal-id uint) (rating uint) (comment (string-ascii 100)))
    (let
        (
            (escrow (unwrap! (map-get? escrow-accounts { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
            (existing-rating (map-get? contractor-ratings { proposal-id: proposal-id, rater: tx-sender }))
            (contractor (get contractor escrow))
            (current-performance (default-to 
                { total-projects: u0, completed-projects: u0, average-rating: u0, total-ratings: u0, on-time-completions: u0 }
                (map-get? contractor-performance { contractor: contractor })
            ))
        )
        (asserts! (is-none existing-rating) ERR-RATING-ALREADY-GIVEN)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (is-eq (get released-amount escrow) (get total-amount escrow)) ERR-STAGE-NOT-READY)
        (map-set contractor-ratings
            { proposal-id: proposal-id, rater: tx-sender }
            { 
                rating: rating,
                comment: comment,
                given-at: stacks-block-height
            }
        )
        (let
            (
                (new-total-ratings (+ (get total-ratings current-performance) u1))
                (new-average (/ (+ (* (get average-rating current-performance) (get total-ratings current-performance)) rating) new-total-ratings))
                (is-on-time (< (get created-at escrow) (+ (get created-at escrow) (var-get auto-release-delay))))
            )
            (map-set contractor-performance
                { contractor: contractor }
                {
                    total-projects: (+ (get total-projects current-performance) u1),
                    completed-projects: (+ (get completed-projects current-performance) u1),
                    average-rating: new-average,
                    total-ratings: new-total-ratings,
                    on-time-completions: (if is-on-time (+ (get on-time-completions current-performance) u1) (get on-time-completions current-performance))
                }
            )
        )
        (ok true)
    )
)

(define-public (emergency-release-funds (proposal-id uint))
    (let
        (
            (escrow (unwrap! (map-get? escrow-accounts { proposal-id: proposal-id }) ERR-ESCROW-NOT-FOUND))
            (current-stage-id (get current-stage escrow))
            (stage (unwrap! (map-get? escrow-stages { proposal-id: proposal-id, stage-id: current-stage-id }) ERR-INVALID-STAGE))
            (time-elapsed (- stacks-block-height (get completion-time stage)))
        )
        (asserts! (get completed stage) ERR-STAGE-NOT-READY)
        (asserts! (> time-elapsed (var-get auto-release-delay)) ERR-STAGE-NOT-READY)
        (asserts! (not (get dispute-active escrow)) ERR-DISPUTE-ACTIVE)
        (try! (check-and-release-stage-funds proposal-id current-stage-id))
        (ok true)
    )
)

(define-read-only (get-escrow-account (proposal-id uint))
    (ok (map-get? escrow-accounts { proposal-id: proposal-id }))
)

(define-read-only (get-escrow-stage (proposal-id uint) (stage-id uint))
    (ok (map-get? escrow-stages { proposal-id: proposal-id, stage-id: stage-id }))
)

(define-read-only (get-escrow-balance (proposal-id uint))
    (ok (map-get? escrow-funds { proposal-id: proposal-id }))
)

(define-read-only (get-contractor-performance (contractor principal))
    (ok (map-get? contractor-performance { contractor: contractor }))
)

(define-read-only (get-dispute-case (proposal-id uint))
    (ok (map-get? dispute-cases { proposal-id: proposal-id }))
)

(define-read-only (get-contractor-rating (proposal-id uint) (rater principal))
    (ok (map-get? contractor-ratings { proposal-id: proposal-id, rater: rater }))
)

;; Dynamic Budget Reallocation System Constants
(define-constant ERR-SURPLUS-NOT-FOUND (err u114))
(define-constant ERR-REALLOCATION-ACTIVE (err u115))
(define-constant ERR-INVALID-PRIORITY (err u116))
(define-constant ERR-INSUFFICIENT-VOTES (err u117))
(define-constant ERR-REALLOCATION-EXPIRED (err u118))

;; Surplus fund tracking from completed/failed proposals
(define-map budget-surplus
    { period: uint }
    { 
        total-surplus: uint,
        allocated-surplus: uint,
        collection-start: uint
    }
)

;; Category priority weights for reallocation
(define-map category-priorities
    { category: (string-ascii 20) }
    { 
        priority-weight: uint,
        last-allocation: uint,
        demand-score: uint
    }
)

;; Reallocation proposals for surplus funds
(define-map reallocation-proposals
    { reallocation-id: uint }
    {
        proposer: principal,
        target-categories: (list 5 (string-ascii 20)),
        allocation-percentages: (list 5 uint),
        surplus-amount: uint,
        votes-for: uint,
        votes-against: uint,
        status: (string-ascii 20),
        created-at: uint,
        execution-deadline: uint
    }
)

;; Reallocation voting tracking
(define-map reallocation-votes
    { voter: principal, reallocation-id: uint }
    { vote-type: (string-ascii 10), weight: uint }
)

;; Historical reallocation data
(define-map reallocation-history
    { period: uint, category: (string-ascii 20) }
    { 
        amount-received: uint,
        source-surplus: uint,
        efficiency-score: uint
    }
)

;; Dynamic priority calculations
(define-map category-demand
    { category: (string-ascii 20) }
    {
        pending-proposals: uint,
        total-requested: uint,
        success-rate: uint,
        last-updated: uint
    }
)

;; System configuration variables
(define-data-var reallocation-count uint u0)
(define-data-var current-period uint u1)
(define-data-var reallocation-threshold uint u50000)
(define-data-var voting-duration uint u1008)
(define-data-var max-reallocation-categories uint u5)

;; Collect surplus from completed or failed proposals
(define-public (collect-proposal-surplus (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-PROPOSAL))
            (escrow (map-get? escrow-accounts { proposal-id: proposal-id }))
            (current-surplus (default-to 
                { total-surplus: u0, allocated-surplus: u0, collection-start: stacks-block-height }
                (map-get? budget-surplus { period: (var-get current-period) })
            ))
        )
        ;; Check if proposal is completed or expired
        (asserts! (or 
            (is-eq (get status proposal) "completed")
            (> (- stacks-block-height (get created-at proposal)) (* (var-get voting-period) u2))
        ) ERR-VOTING-CLOSED)
        
        (let
            (
                (surplus-amount (calculate-surplus-amount proposal escrow))
            )
            (if (> surplus-amount u0)
                (begin
                    ;; Update budget surplus for current period
                    (map-set budget-surplus
                        { period: (var-get current-period) }
                        {
                            total-surplus: (+ (get total-surplus current-surplus) surplus-amount),
                            allocated-surplus: (get allocated-surplus current-surplus),
                            collection-start: (get collection-start current-surplus)
                        }
                    )
                    ;; Update proposal status to surplus-collected
                    (map-set proposals
                        { proposal-id: proposal-id }
                        (merge proposal { status: "surplus-collected" })
                    )
                    (ok surplus-amount)
                )
                (ok u0)
            )
        )
    )
)

;; Calculate surplus amount from a proposal
(define-private (calculate-surplus-amount 
    (proposal { creator: principal, title: (string-ascii 50), description: (string-ascii 500), amount: uint, votes: uint, status: (string-ascii 20), created-at: uint })
    (escrow-opt (optional { total-amount: uint, released-amount: uint, stage-count: uint, current-stage: uint, contractor: principal, dispute-active: bool, created-at: uint })))
    (match escrow-opt
        escrow-data
        ;; If escrow exists, surplus is unreleased funds
        (- (get total-amount escrow-data) (get released-amount escrow-data))
        ;; If no escrow, check if proposal failed or expired
        (if (or 
            (is-eq (get status proposal) "failed")
            (< (get votes proposal) u10)) ;; Assume minimum 10 votes needed
            (get amount proposal)
            u0
        )
    )
)

;; Set category priority weights
(define-public (set-category-priority (category (string-ascii 20)) (weight uint))
    (begin
        (asserts! (and (>= weight u1) (<= weight u100)) ERR-INVALID-PRIORITY)
        (map-set category-priorities
            { category: category }
            {
                priority-weight: weight,
                last-allocation: u0,
                demand-score: (calculate-demand-score category)
            }
        )
        (ok true)
    )
)

;; Calculate demand score for a category
(define-private (calculate-demand-score (category (string-ascii 20)))
    (let
        (
            (demand-data (default-to 
                { pending-proposals: u0, total-requested: u0, success-rate: u50, last-updated: u0 }
                (map-get? category-demand { category: category })
            ))
            (category-budget (map-get? category-budgets { category: category }))
        )
        (match category-budget
            budget-info
            (let
                (
                    (utilization-rate (if (> (get cap budget-info) u0)
                        (/ (* (get used budget-info) u100) (get cap budget-info))
                        u0
                    ))
                )
                ;; Higher demand score = more pending proposals + higher utilization + lower success rate
                (+ (get pending-proposals demand-data) 
                   (/ utilization-rate u2)
                   (- u100 (get success-rate demand-data)))
            )
            u50 ;; Default demand score
        )
    )
)

;; Create reallocation proposal
(define-public (create-reallocation-proposal 
    (target-categories (list 5 (string-ascii 20)))
    (allocation-percentages (list 5 uint)))
    (let
        (
            (new-reallocation-id (+ (var-get reallocation-count) u1))
            (current-surplus (unwrap! (map-get? budget-surplus { period: (var-get current-period) }) ERR-SURPLUS-NOT-FOUND))
            (available-surplus (- (get total-surplus current-surplus) (get allocated-surplus current-surplus)))
            (total-percentage (fold + allocation-percentages u0))
        )
        ;; Validate inputs
        (asserts! (> available-surplus (var-get reallocation-threshold)) ERR-INSUFFICIENT-FUNDS)
        (asserts! (is-eq total-percentage u100) ERR-INVALID-PRIORITY)
        (asserts! (and (> (len target-categories) u0) (<= (len target-categories) (var-get max-reallocation-categories))) ERR-INVALID-STAGE)
        
        ;; Create reallocation proposal
        (map-set reallocation-proposals
            { reallocation-id: new-reallocation-id }
            {
                proposer: tx-sender,
                target-categories: target-categories,
                allocation-percentages: allocation-percentages,
                surplus-amount: available-surplus,
                votes-for: u0,
                votes-against: u0,
                status: "active",
                created-at: stacks-block-height,
                execution-deadline: (+ stacks-block-height (var-get voting-duration))
            }
        )
        (var-set reallocation-count new-reallocation-id)
        (ok new-reallocation-id)
    )
)

;; Vote on reallocation proposal
(define-public (vote-reallocation (reallocation-id uint) (support bool))
    (let
        (
            (reallocation (unwrap! (map-get? reallocation-proposals { reallocation-id: reallocation-id }) ERR-NO-PROPOSAL))
            (existing-vote (map-get? reallocation-votes { voter: tx-sender, reallocation-id: reallocation-id }))
            (vote-weight (calculate-voter-weight tx-sender))
        )
        ;; Validate voting conditions
        (asserts! (is-eq (get status reallocation) "active") ERR-VOTING-CLOSED)
        (asserts! (< stacks-block-height (get execution-deadline reallocation)) ERR-REALLOCATION-EXPIRED)
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        
        ;; Record vote
        (map-set reallocation-votes
            { voter: tx-sender, reallocation-id: reallocation-id }
            { 
                vote-type: (if support "for" "against"),
                weight: vote-weight
            }
        )
        
        ;; Update vote counts
        (map-set reallocation-proposals
            { reallocation-id: reallocation-id }
            (merge reallocation {
                votes-for: (if support 
                    (+ (get votes-for reallocation) vote-weight)
                    (get votes-for reallocation)
                ),
                votes-against: (if support
                    (get votes-against reallocation)
                    (+ (get votes-against reallocation) vote-weight)
                )
            })
        )
        (ok true)
    )
)

;; Calculate voter weight based on participation history - simple implementation for now
(define-private (calculate-voter-weight (voter principal))
    u1
)

;; Execute approved reallocation
(define-public (execute-reallocation (reallocation-id uint))
    (let
        (
            (reallocation (unwrap! (map-get? reallocation-proposals { reallocation-id: reallocation-id }) ERR-NO-PROPOSAL))
            (current-surplus (unwrap! (map-get? budget-surplus { period: (var-get current-period) }) ERR-SURPLUS-NOT-FOUND))
        )
        ;; Validate execution conditions
        (asserts! (is-eq (get status reallocation) "active") ERR-VOTING-CLOSED)
        (asserts! (> (get votes-for reallocation) (get votes-against reallocation)) ERR-INSUFFICIENT-VOTES)
        (asserts! (>= stacks-block-height (get execution-deadline reallocation)) ERR-REALLOCATION-EXPIRED)
        
        ;; Execute reallocation
        (let
            (
                (allocation-result (process-category-allocations 
                    (get target-categories reallocation)
                    (get allocation-percentages reallocation)
                    (get surplus-amount reallocation)
                ))
            )
            ;; Update surplus allocation
            (map-set budget-surplus
                { period: (var-get current-period) }
                (merge current-surplus {
                    allocated-surplus: (+ (get allocated-surplus current-surplus) (get surplus-amount reallocation))
                })
            )
            
            ;; Mark reallocation as executed
            (map-set reallocation-proposals
                { reallocation-id: reallocation-id }
                (merge reallocation { status: "executed" })
            )
            (ok true)
        )
    )
)

;; Process category allocations
(define-private (process-category-allocations 
    (categories (list 5 (string-ascii 20)))
    (percentages (list 5 uint))
    (total-amount uint))
    (let
        (
            (allocation-pairs (zip-categories-percentages categories percentages))
        )
        (fold process-single-allocation allocation-pairs total-amount)
    )
)

;; Helper function to zip categories and percentages
(define-private (zip-categories-percentages 
    (categories (list 5 (string-ascii 20)))
    (percentages (list 5 uint)))
    (map create-allocation-pair categories percentages)
)

;; Create allocation pair
(define-private (create-allocation-pair (category (string-ascii 20)) (percentage uint))
    { category: category, percentage: percentage }
)

;; Process single category allocation
(define-private (process-single-allocation 
    (allocation { category: (string-ascii 20), percentage: uint })
    (total-amount uint))
    (let
        (
            (category (get category allocation))
            (allocation-amount (/ (* total-amount (get percentage allocation)) u100))
            (current-budget (unwrap! (map-get? category-budgets { category: category }) u0))
        )
        ;; Update category budget cap
        (map-set category-budgets
            { category: category }
            {
                cap: (+ (get cap current-budget) allocation-amount),
                used: (get used current-budget)
            }
        )
        
        ;; Record reallocation history
        (map-set reallocation-history
            { period: (var-get current-period), category: category }
            {
                amount-received: allocation-amount,
                source-surplus: total-amount,
                efficiency-score: (calculate-allocation-efficiency category allocation-amount)
            }
        )
        total-amount
    )
)

;; Calculate allocation efficiency based on demand score
(define-private (calculate-allocation-efficiency (category (string-ascii 20)) (amount uint))
    (let
        (
            (demand-score (calculate-demand-score category))
        )
        (if (> demand-score u0)
            (/ (* amount u100) demand-score)
            u50
        )
    )
)

;; Advance to next budget period
(define-public (advance-budget-period)
    (let
        (
            (current-surplus (map-get? budget-surplus { period: (var-get current-period) }))
        )
        ;; Only advance if current period has been active for sufficient time
        (asserts! (match current-surplus
            surplus-data
            (> (- stacks-block-height (get collection-start surplus-data)) u4032) ;; ~4 weeks
            true
        ) ERR-VOTING-CLOSED)
        
        ;; Initialize new period
        (var-set current-period (+ (var-get current-period) u1))
        (map-set budget-surplus
            { period: (var-get current-period) }
            {
                total-surplus: u0,
                allocated-surplus: u0,
                collection-start: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Read-only functions for budget reallocation data
(define-read-only (get-budget-surplus (period uint))
    (ok (map-get? budget-surplus { period: period }))
)

(define-read-only (get-category-priority (category (string-ascii 20)))
    (ok (map-get? category-priorities { category: category }))
)

(define-read-only (get-reallocation-proposal (reallocation-id uint))
    (ok (map-get? reallocation-proposals { reallocation-id: reallocation-id }))
)

(define-read-only (get-reallocation-vote (voter principal) (reallocation-id uint))
    (ok (map-get? reallocation-votes { voter: voter, reallocation-id: reallocation-id }))
)

(define-read-only (get-reallocation-history (period uint) (category (string-ascii 20)))
    (ok (map-get? reallocation-history { period: period, category: category }))
)

(define-read-only (get-current-period)
    (ok (var-get current-period))
)

(define-read-only (get-available-surplus)
    (let
        (
            (current-surplus (map-get? budget-surplus { period: (var-get current-period) }))
        )
        (match current-surplus
            surplus-data
            (ok (- (get total-surplus surplus-data) (get allocated-surplus surplus-data)))
            (ok u0)
        )
    )
)



