;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname akdjshdkasdhasd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct saida (omega alpha))
;resultado: omega - o número de clique e alpha - o número de independência  

(define-struct nodo (char lds))
;nodo é empty ou é make-nodo (síbolo lista-de-símbolos)



;AQUI VEM O NODO PRO TRABALHO MESMO








;nodos da galera
(define A(make-nodo 'A (list 'B 'D 'E)))
(define B(make-nodo 'B (list 'A 'C 'E)))
(define C(make-nodo 'C (list 'B 'D 'F)))
(define D(make-nodo 'D (list 'A 'C 'F)))
(define E(make-nodo 'E (list 'A 'B 'F)))
(define F(make-nodo 'F (list 'C 'D 'E)))

(define G(make-nodo 'G (list 'H 'I 'J)))
(define H(make-nodo 'H (list 'G 'I 'J)))
(define I(make-nodo 'I (list 'G 'H 'J)))
(define J(make-nodo 'J (list 'G 'H 'I)))

(define K(make-nodo 'K (list 'L 'M)))
(define L(make-nodo 'L (list 'K 'M)))
(define M(make-nodo 'M (list 'K 'L)))

(define U(make-nodo 'U (list 'Y 'X 'V)))
(define V(make-nodo 'V (list 'U 'W)))
(define W(make-nodo 'W (list 'V 'X)))
(define X(make-nodo 'X (list 'U 'W 'Y)))
(define Y(make-nodo 'Y (list 'U 'X)))


;grafos da galera
(define grafo1 (list A B C D E F))
(define grafo2 (list A D E F))
(define grafo3 (list G H I J)) ;esse é k4
(define grafo4 (list K L M)) ;esse é k3
(define grafo5 (list A G K)) ;totalmente desconexo
(define grafo6 (list A B G K)) ;relativamente desconexo
(define grafo7 (list U V W X Y))




;nodos
;(define A (make-nodo 'A (list 'B 'E)))
;(define B (make-nodo 'B (list 'A 'E 'F)))
;(define C (make-nodo 'C (list 'D 'E)))
;(define D (make-nodo 'D (list 'C 'F))) 
;(define E (make-nodo 'E (list 'A 'B 'C 'F)))
;(define F (make-nodo 'F (list 'B 'D 'E 'G)))
;(define G (make-nodo 'G (list 'F)))

;exemplo grafos
;(define G1  (list A B C D E F G))


;calcula o comprimento, quantidade de vértices, do grafo
(define (comprimento lds)
  (cond
    [(empty? lds) 0]
    [else (+ 1 (comprimento (rest lds)))]))

; vizinhos : símbolo, grafo -> lista-de-símbolos
; (vizinhos n G) retorna todos os nodos no grafo G  que recebem arestas de n
;(define (vizinhos n G)
;  (cond
; erro, não achou nodo
;    [(empty? G) empty]
; achou nodo, retorna nodos adjacentes
;    [(equal? n (first (first G))) (rest (first G))]
; não é o nodo atual, continua busca no resto da lista
;    [else (vizinhos n (rest G))]))


; conectados-larg? : símbolo símbolo grafo -> boolean
;(define (conectados-larg? a b G) (busca-larg (list a) (list ) b G))
; busca-larg : lista-simb, simb, lista-simb, grafo -> boolean
;(define (busca-larg la p b G)
;  (cond ; lista vazia?
;    [(empty? la) false]
; destino pertence à lista de nodos atual?
;    [(member b la) true]
; repete busca nos vizinhos dos vizinhos
;    [else (busca-larg (todos-vizinhos la p G) (append la p) b G)]))
; todos-vizinhos: lista-simb, lista-simb, grafo -> lista-simb
;(define (todos-vizinhos la p G)
; (cond
;    [(empty? la) empty]
;    [else (append (filter (lambda (x) (not (member x p))) (vizinhos (first la) G)) (todos-vizinhos (rest la) p G))]))

;procura um símoblo numa lista ---> boolean
(define (procura sim lds)
  (cond
    [(empty? lds) false]
    [(symbol=? sim (first lds)) true]
    [else (procura sim (rest lds))]))

;descobre seu tem um vizinho ----> boolean
(define (vizinho nodo grafor)
  (cond
    [(empty? grafor) true]
    [(procura (nodo-char nodo) (nodo-lds (first grafor))) true] ;(append (cons nodo (cons (first grafo) empty)) (vizinho nodo (rest grafo)))]
    [else (vizinho nodo (rest grafor))]))
;grafo é o resultante

;troca ----> grafo
(define (frescura lds)
  (cond
    [(empty? lds) false]
    [else (append (rest lds) (cons (first lds) empty))]))

;acha todos os vizinhos ----> boolean
;verifica se um nodo é vizinho de todos os outros
;teste grafo completo
(define (acha-vizinhos nodo grafo)
  (cond
    [(empty? grafo) true]
    [else (and (procura (nodo-char nodo) (nodo-lds (first grafo)))
                  (acha-vizinhos nodo (rest grafo)))])) 

;constróis
;ACHA UM FODENDO CLIQUE, apenas
(define (teste grafo resultado)
  (cond
    [(empty? grafo) (cons resultado empty)]
    [(acha-vizinhos (first grafo) resultado)  (teste (rest grafo) (cons (first grafo) resultado))]
    [else (teste (rest grafo) resultado)]))

;cu
(define (na-pista cont grafo)
  (cond
    [(= cont 0) empty]
    [else (append (teste grafo empty)
                  (na-pista (- cont 1) (frescura grafo)))]))



;maior dos cliques
(define (maior resultado maiorAtual)
  (cond
   [(empty? resultado) maiorAtual] 
   [(> (comprimento (first resultado)) (comprimento maiorAtual)) (maior (rest resultado) (first resultado))]
   [else (maior (rest resultado) maiorAtual)]))


;PROBLEMA DOS CLIQUES: RESOLVIDO
(define (omega grafo)
  (list 'Número 'de 'clique:  (comprimento (maior (na-pista (comprimento grafo) grafo) empty))))

;--------------------------------------------------------------------------------------

;acha complemento do grafo

;acha todos os vizinhos ----> boolean
;verifica se um nodo é vizinho de todos os outros
;teste grafo completo
(define (não-acha-vizinhos nodo grafo)
  (cond
    [(empty? grafo) true]
    [else (and (not (procura (nodo-char nodo) (nodo-lds (first grafo))))
                  (não-acha-vizinhos nodo (rest grafo)))])) 


;constróis
;ACHA UM FODENDO CONJUNTO INDEPENDENTE, apenas
(define (não-teste grafo resultado)
  (cond
    [(empty? grafo) (cons resultado empty)]
    [(não-acha-vizinhos (first grafo) resultado)  (não-teste (rest grafo) (cons (first grafo) resultado))]
    [else (não-teste (rest grafo) resultado)]))

;não-tão-cu-assim
(define (tava-na-pista cont grafo)
  (cond
    [(= cont 0) empty]
    [else (append (não-teste grafo empty)
                  (tava-na-pista (- cont 1) (frescura grafo)))]))

;PROBLEMA DOS CONJUNTO INDEPENDENTE: RESOLVIDO
(define (alpha grafo)
  (list 'Número 'de 'independência:  (comprimento (maior (tava-na-pista (comprimento grafo) grafo) empty))))

(define (racket-s2 grafo)
  (make-saida (omega grafo) (alpha grafo)))



