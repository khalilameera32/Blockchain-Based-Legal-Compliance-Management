;; Entity Verification Contract
;; Validates regulated businesses

(define-data-var admin principal tx-sender)

;; Entity status: 0 = unverified, 1 = verified, 2 = suspended
(define-map entities
  { entity-id: (string-ascii 64) }
  {
    name: (string-ascii 100),
    principal: principal,
    status: uint,
    verification-date: uint,
    last-updated: uint
  }
)

(define-read-only (get-entity (entity-id (string-ascii 64)))
  (map-get? entities { entity-id: entity-id })
)

(define-read-only (is-entity-verified (entity-id (string-ascii 64)))
  (let ((entity (get-entity entity-id)))
    (if (is-some entity)
      (is-eq (get status (unwrap-panic entity)) u1)
      false
    )
  )
)

(define-public (register-entity (entity-id (string-ascii 64)) (name (string-ascii 100)))
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (if (is-none (get-entity entity-id))
      (begin
        (map-set entities
          { entity-id: entity-id }
          {
            name: name,
            principal: tx-sender,
            status: u0,
            verification-date: u0,
            last-updated: current-time
          }
        )
        (ok true)
      )
      (err u1) ;; Entity already exists
    )
  )
)

(define-public (verify-entity (entity-id (string-ascii 64)))
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (if (is-eq tx-sender (var-get admin))
      (match (get-entity entity-id)
        entity (begin
          (map-set entities
            { entity-id: entity-id }
            (merge entity {
              status: u1,
              verification-date: current-time,
              last-updated: current-time
            })
          )
          (ok true)
        )
        (err u2) ;; Entity not found
      )
      (err u3) ;; Not authorized
    )
  )
)

(define-public (suspend-entity (entity-id (string-ascii 64)))
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (if (is-eq tx-sender (var-get admin))
      (match (get-entity entity-id)
        entity (begin
          (map-set entities
            { entity-id: entity-id }
            (merge entity {
              status: u2,
              last-updated: current-time
            })
          )
          (ok true)
        )
        (err u2) ;; Entity not found
      )
      (err u3) ;; Not authorized
    )
  )
)

(define-public (set-admin (new-admin principal))
  (if (is-eq tx-sender (var-get admin))
    (begin
      (var-set admin new-admin)
      (ok true)
    )
    (err u3) ;; Not authorized
  )
)
