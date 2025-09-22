;; multi-class-nft.clar
;; Multi-class (rarity-tier) NFT contract for Stacks (Clarity / SIP-009)
;; - Admin creates classes (tiers) with max supply and mint price
;; - Public minting (paying STX) and admin minting (free)
;; - Token -> class mapping and optional metadata per token
;; - Royalties info stored per class (informational only; marketplaces may read it)

(define-non-fungible-token multi-nft uint)

;; ------------------------
;; Errors
;; ------------------------
(define-constant ERR_NOT_ADMIN u100)
(define-constant ERR_CLASS_NOT_FOUND u101)
(define-constant ERR_CLASS_INACTIVE u102)
(define-constant ERR_CLASS_SOLD_OUT u103)
(define-constant ERR_BAD_PRICE u104)
(define-constant ERR_BAD_ARGS u105)
(define-constant ERR_NO_FUNDS u106)
(define-constant ERR_MINT_FAILED u107)
(define-constant ERR_NOT_OWNER u108)

;; ------------------------
;; Admin / config
;; ------------------------
(define-data-var admin principal tx-sender)
(define-data-var contract-treasury principal tx-sender) ;; where mint fees can be withdrawn to; defaults to deployer

;; token counter (incrementing token-id)
(define-data-var token-counter uint u0)

;; class counter (incremental class id)
(define-data-var class-counter uint u0)

;; ------------------------
;; Class structure
;; ------------------------
;; Stored per class-id:
;; { name: (string-ascii 32), max-supply: uint, minted: uint,
;;   price: uint (microSTX per mint), base-uri: (string-ascii 200),
;;   active: bool, royalty-bps: uint, royalty-recipient: (optional principal) }
(define-map classes
  { class-id: uint }
  {
    name: (string-ascii 32),
    max-supply: uint,
    minted: uint,
    price: uint,
    base-uri: (string-ascii 200),
    active: bool,
    royalty-bps: uint,
    royalty-recipient: (optional principal)
  })

;; token -> class mapping and optional per-token metadata URI
(define-map token-class
  { token-id: uint }
  { class-id: uint })

(define-map token-metadata
  { token-id: uint }
  { uri: (optional (string-ascii 200)) })

;; ------------------------
;; Events (print for indexers)
;; ------------------------
(define-private (ev-class-created (cid uint) (name (string-ascii 32)) (max uint) (price uint))
  (print { event: "class-created", class_id: cid, name: name, max_supply: max, price: price }))

(define-private (ev-class-updated (cid uint))
  (print { event: "class-updated", class_id: cid }))

(define-private (ev-minted (token-id uint) (to principal) (class-id uint) (paid uint))
  (print { event: "minted", token_id: token-id, to: to, class_id: class-id, paid: paid }))

(define-private (ev-admin-mint (token-id uint) (to principal) (class-id uint))
  (print { event: "admin-minted", token_id: token-id, to: to, class_id: class-id }))

(define-private (ev-withdrawn (to principal) (amount uint))
  (print { event: "withdrawn", to: to, amount: amount }))

;; ------------------------
;; Helpers
;; ------------------------
(define-read-only (is-admin (p principal)) (is-eq p (var-get admin)))

(define-private (check-admin)
  (if (is-admin tx-sender)
    (ok true)
    (err ERR_NOT_ADMIN)))

(define-public (set-admin (p principal))
  (begin
    (try! (check-admin))
    (var-set admin p)
    (ok true)))

(define-public (set-treasury (p principal))
  (begin
    (try! (check-admin))
    (var-set contract-treasury p)
    (ok true)))

;; ------------------------
;; Admin: create a new class (tier)
;; returns new class-id
;; ------------------------
(define-public (create-class
  (name (string-ascii 32))
  (max-supply uint)
  (price uint) ;; microSTX per mint; 0 => free
  (base-uri (string-ascii 200))
  (royalty-bps uint) ;; royalty in bps e.g., 500 = 5%
  (royalty-recipient (optional principal)))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (> max-supply u0) (err ERR_BAD_ARGS))
    (asserts! (<= royalty-bps u10000) (err ERR_BAD_ARGS))
    (let ((cid (+ (var-get class-counter) u1)))
      (var-set class-counter cid)
      (map-set classes { class-id: cid }
        {
          name: name,
          max-supply: max-supply,
          minted: u0,
          price: price,
          base-uri: base-uri,
          active: true,
          royalty-bps: royalty-bps,
          royalty-recipient: royalty-recipient
        })
      (ev-class-created cid name max-supply price)
      (ok cid))))

;; ------------------------
;; Admin: update class properties (partial updates)
;; ------------------------
(define-public (update-class
  (cid uint)
  (name (optional (string-ascii 32)))
  (max-supply (optional uint))
  (price (optional uint))
  (base-uri (optional (string-ascii 200)))
  (active (optional bool))
  (royalty-bps (optional uint))
  (royalty-recipient (optional (optional principal))))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (let ((c (map-get? classes { class-id: cid })))
      (asserts! (is-some c) (err ERR_CLASS_NOT_FOUND))
      (let ((rec (unwrap-panic c)))
        (let ((cur-name (get name rec))
              (cur-max (get max-supply rec))
              (cur-minted (get minted rec))
              (cur-price (get price rec))
              (cur-base (get base-uri rec))
              (cur-active (get active rec))
              (cur-royalty (get royalty-bps rec))
              (cur-roy-rec (get royalty-recipient rec)))
          ;; validate new max-supply not less than minted
          (if (is-some max-supply)
            (asserts! (>= (unwrap-panic max-supply) cur-minted) (err ERR_BAD_ARGS))
            true)
          (map-set classes { class-id: cid }
            {
              name: (if (is-some name) (unwrap-panic name) cur-name),
              max-supply: (if (is-some max-supply) (unwrap-panic max-supply) cur-max),
              minted: cur-minted,
              price: (if (is-some price) (unwrap-panic price) cur-price),
              base-uri: (if (is-some base-uri) (unwrap-panic base-uri) cur-base),
              active: (if (is-some active) (unwrap-panic active) cur-active),
              royalty-bps: (if (is-some royalty-bps) (unwrap-panic royalty-bps) cur-royalty),
              royalty-recipient: (if (is-some royalty-recipient) (unwrap-panic royalty-recipient) cur-roy-rec)
            })
          (ev-class-updated cid)
          (ok true))))))

;; ------------------------
;; Internal: mint helper (creates token id, mints NFT, records mapping)
;; returns token-id
;; ------------------------
(define-private (do-mint (to principal) (cid uint) (metadata (optional (string-ascii 200))) (paid uint))
  (let ((next (+ (var-get token-counter) u1)))
    ;; attempt to mint NFT
    (match (nft-mint? multi-nft next to)
      success (begin
          (var-set token-counter next)
          (map-set token-class { token-id: next } { class-id: cid })
          (map-set token-metadata { token-id: next } { uri: metadata })
          (ev-minted next to cid paid)
          (ok next))
      error (err ERR_MINT_FAILED))))

;; ------------------------
;; Public mint (payable) caller pays class.price to contract; receives token
;; metadata param optional (recommended to pass token metadata or CID-specific off-chain URI)
;; ------------------------
(define-public (mint (cid uint) (metadata (optional (string-ascii 200))))
  (let ((class (unwrap! (map-get? classes { class-id: cid }) (err ERR_CLASS_NOT_FOUND))))
    (asserts! (get active class) (err ERR_CLASS_INACTIVE))
    (let ((minted (get minted class))
          (max (get max-supply class))
          (price (get price class)))
      (asserts! (< minted max) (err ERR_CLASS_SOLD_OUT))
      ;; require payment if price > 0
      (if (> price u0)
          ;; paid mint - first transfer payment, then mint
          (let ((transfer-result (unwrap! (stx-transfer? price tx-sender (as-contract tx-sender)) 
                                        (err ERR_NO_FUNDS))))
            (let ((mint-result (do-mint tx-sender cid metadata price)))
              (if (is-ok mint-result)
                  (begin
                    (map-set classes { class-id: cid }
                      (merge class { minted: (+ minted u1) }))
                    mint-result)
                  mint-result)))
          ;; free mint
          (let ((mint-result (do-mint tx-sender cid metadata u0)))
            (if (is-ok mint-result)
                (begin
                  (map-set classes { class-id: cid }
                    (merge class { minted: (+ minted u1) }))
                  mint-result)
                mint-result))))))

;; ------------------------
;; Admin mint (no payment) to any address
;; ------------------------
(define-public (admin-mint (cid uint) (to principal) (metadata (optional (string-ascii 200))))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (let ((class-data (unwrap! (map-get? classes { class-id: cid }) (err ERR_CLASS_NOT_FOUND))))
      (asserts! (get active class-data) (err ERR_CLASS_INACTIVE))
      (let ((minted (get minted class-data))
            (max (get max-supply class-data)))
        (asserts! (< minted max) (err ERR_CLASS_SOLD_OUT))
        (let ((mint-result (do-mint to cid metadata u0)))
          (if (is-ok mint-result)
              (begin
                (map-set classes { class-id: cid }
                  (merge class-data { minted: (+ minted u1) }))
                (let ((token-id (unwrap-panic mint-result)))
                  (ev-admin-mint token-id to cid)
                  mint-result))
              mint-result))))))

;; ------------------------
;; Withdraw collected mint fees (admin only)
;; ------------------------
(define-public (withdraw (to principal) (amount uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (> amount u0) (err ERR_BAD_ARGS))
    ;; ensure contract has enough STX
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) (err ERR_NO_FUNDS))
    (try! (stx-transfer? amount (as-contract tx-sender) to))
    (ev-withdrawn to amount)
    (ok true)))

;; ------------------------
;; Views
;; ------------------------
(define-read-only (get-class (cid uint))
  (map-get? classes { class-id: cid }))

(define-read-only (get-class-count)
  (var-get class-counter))

(define-read-only (get-token-class (tid uint))
  (map-get? token-class { token-id: tid }))

(define-read-only (get-token-metadata (tid uint))
  (map-get? token-metadata { token-id: tid }))

(define-read-only (total-minted-in-class (cid uint))
  (let ((c (map-get? classes { class-id: cid })))
    (if (is-none c) (err ERR_CLASS_NOT_FOUND)
      (ok (get minted (unwrap-panic c))))))

(define-read-only (get-admin) (var-get admin))
(define-read-only (get-treasury) (var-get contract-treasury))
