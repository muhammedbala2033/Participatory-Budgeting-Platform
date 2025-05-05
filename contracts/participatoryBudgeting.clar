(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-EXISTS (err u101))
(define-constant ERR-NO-PROPOSAL (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))

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