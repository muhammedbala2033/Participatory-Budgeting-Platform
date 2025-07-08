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
(define-data-var auto-release-delay uint u2016)Zz

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