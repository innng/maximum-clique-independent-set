;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |trab (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct nodo (id ldv))
;um nodo é empty ou make-nodo (simbolo lista-de-simbolos)

(define-struct resultado (alpha omega))
;um resultado é make-resultado onde alpha é o tamanho do maior conjunto independente e omega é o tamanho do maior clique

;um grafo é ou empty ou (cons A B), onde:
;A é um nodo
;B é um grafo


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funções gerais:

;na-lista: Símbolo Lista-de-símbolos -> Booleano
;verifica se o elemento procurado está na lista
(define (na-lista simbolo lds)
  (cond
    [(empty? lds) false]
    [(symbol=? simbolo (first lds)) true]
    [else (na-lista simbolo (rest lds))]))


;comprimento: Grafo -> Número
;Retorna o numero de elementos do grafo
(define (comprimento ldn)
  (cond
    [(empty? ldn) 0]
    [else (+ 1 (comprimento (rest ldn)))]))


;trocatroca: Grafo -> Lista-de-nodos
;Passa o primeiro elemento da lista pra ultima posiçao
(define (troca ldn)
  (append (rest ldn) (cons (first ldn) empty)))


;maiores: Grafo -> Lista de números
;Cria uma lista com o comprimento de todos os sub-grafos encontrados
(define (maiores ldn)
  (cond
    [(empty? ldn) empty]
    [else (cons (comprimento (first ldn)) (maiores (rest ldn)))]))


;maior: Lista de numeros Numero -> Numero
;Encontra o maior numero na lista
(define (maior ldn n)
  (cond
    [(empty? ldn) n]
    [(> (first ldn) n) (maior (rest ldn) (first ldn))]
    [else (maior (rest ldn) n)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funções para cliques:

;testa-clique: Nodo Grafo -> Booleano
;Verifica se o nodo tá na lista de vizinhos de todos nodos do grafo
(define (testa-clique nodo grafo)
  (cond
    [(empty? grafo) true]
    [(and (na-lista (nodo-id nodo) (nodo-ldv (first grafo)))  
          (testa-clique nodo (rest grafo))) true]
    [else false]))

;acha-cliques: Grafo Grafo -> Grafo
;Constroi o "grafo" res com os nodos que são adjacentes a todos os outros
(define (acha-cliques ldn res)
  (cond
    [(empty? ldn) (cons res empty)]
    [(testa-clique (first ldn) res) (acha-cliques (rest ldn) (cons (first ldn) res))]
    [else (acha-cliques (rest ldn) res)]))


;todos-cliques: Grafo Número -> Lista de grafos
;Cria uma lista com todos os cliques do grafo
(define (todos ldn n)
  (cond
    [(= n 0) empty]
    [else (append (acha-cliques ldn empty) (todos (troca ldn) (- n 1)))]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funções para conjuntos independentes

;testa-ci: Nodo Grafo -> Booleano
;Verifica se o nodo não tá na lista de vizinhos de todos nodos do grafo
(define (testa-ci nodo grafo)
  (cond
    [(empty? grafo) true]
    [(and (not (na-lista (nodo-id nodo) (nodo-ldv (first grafo))))  
          (testa-ci nodo (rest grafo))) true]
    [else false]))

;acha-cis: Grafo Grafo -> Grafo
;Constroi o "grafo" res com os nodos que não são adjacentes a nenhum outro
(define (acha-cis ldn res)
  (cond
    [(empty? ldn) (cons res empty)]
    [(testa-ci (first ldn) res) (acha-cis (rest ldn) (cons (first ldn) res))]
    [else (acha-cis (rest ldn) res)]))

;todos2: Grafo Número -> Lista de grafos
;Cria uma lista com todos os conjuntos independentes do grafo
(define (todos2 ldn n)
  (cond
    [(= n 0) empty]
    [else (append (acha-cis ldn empty) (todos2 (troca ldn) (- n 1)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Solução:

;trab-grafos: Grafo -> Resultado
;Cria uma estrutura resultado com o alpha e o omega do grafo
(define (trab-grafos grafo)
  (make-resultado (maior (maiores (todos2 grafo (comprimento grafo))) 0)
                  (maior (maiores (todos grafo (comprimento grafo)))0)))

(trab-grafos grafo7)