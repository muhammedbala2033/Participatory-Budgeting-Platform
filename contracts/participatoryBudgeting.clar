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