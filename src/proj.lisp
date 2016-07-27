; *****************************************************************************
;                         Inteligencia Artificial
;                       2015/2016 - Epoca Especial
;
;                           Helena Cruz 78190
; *****************************************************************************

(defconstant +INFINITY+ most-positive-fixnum)

; *****************************************************************************
;                               ESTRUTURAS DE DADOS
; *****************************************************************************

(defstruct estado
  pontos
  pecas-por-colocar
  pecas-colocadas
  tabuleiro)

(defstruct problema
  estado-inicial
  solucao
  accoes
  resultado
  custo-caminho)

(defstruct no
  estado
  accoes
  custo)

; *****************************************************************************
;                                     ACCAO
; *****************************************************************************

(defun cria-accao (coluna peca)
  (cons coluna peca))

(defun accao-coluna (accao)
  (car accao))

(defun accao-peca (accao)
  (cdr accao))


; *****************************************************************************
;                                TABULEIRO
; *****************************************************************************

(defun cria-tabuleiro ()
  (make-array '(18 10)))

(defun copia-tabuleiro (tabuleiro)
  (let ((tabuleiro-novo (cria-tabuleiro)))
    (dotimes (i 18)
      (dotimes (j 10)
        (setf (aref tabuleiro-novo i j) (aref tabuleiro i j))))
    tabuleiro-novo))

(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
  (aref tabuleiro linha coluna))

(defun tabuleiro-altura-coluna (tabuleiro coluna)
  (let ((altura 0))
    (dotimes (linha 18)
      (if (tabuleiro-preenchido-p tabuleiro linha coluna)
        (setf altura (1+ linha))))
    altura))

(defun tabuleiro-linha-inferior-vazia-p (tabuleiro)
  (dotimes (coluna 10)
    (if (tabuleiro-preenchido-p tabuleiro 0 coluna)
      (return-from tabuleiro-linha-inferior-vazia-p nil)))
  T)

(defun tabuleiro-duplica-linha (tabuleiro)
  (let ((novo-tabuleiro (copia-tabuleiro tabuleiro))
        (linha 17))
    (loop while (> linha 0) do
      (dotimes (coluna 10)
        (setf (aref novo-tabuleiro linha coluna)
              (aref novo-tabuleiro (1- linha) coluna)))
      (decf linha))
    novo-tabuleiro))

(defun tabuleiro-linha-inferior-duplicada (tabuleiro)
  (if (tabuleiro-linha-inferior-vazia-p tabuleiro)
    (return-from tabuleiro-linha-inferior-duplicada tabuleiro)
    (tabuleiro-duplica-linha tabuleiro)))

(defun tabuleiro-linha-completa-p (tabuleiro linha)
  (dotimes (coluna 10)
    (if (not (tabuleiro-preenchido-p tabuleiro linha coluna))
      (return-from tabuleiro-linha-completa-p nil)))
  T)

(defun tabuleiro-coluna-buracos (tabuleiro coluna)
  (let ((buracos 0))
    (dotimes (linha 17)
      (if (and (not (tabuleiro-preenchido-p tabuleiro linha coluna))
               (tabuleiro-preenchido-p tabuleiro (1+ linha) coluna))
        (incf buracos)))
    buracos))

(defun linha-coluna-dentro-limites-p (linha coluna)
  (and (>= linha 0) (< linha 18) (>= coluna 0) (< coluna 10)))

(defun tabuleiro-preenche! (tabuleiro linha coluna)
  (if (linha-coluna-dentro-limites-p linha coluna)
    (setf (aref tabuleiro linha coluna) T)))

(defun tabuleiro-despreenche! (tabuleiro linha coluna)
  (setf (aref tabuleiro linha coluna) nil))

(defun tabuleiro-remove-linha! (tabuleiro linha)
  (loop while (< linha 17) do
    (dotimes (coluna 10)
      (setf (aref tabuleiro linha coluna)
            (aref tabuleiro (1+ linha) coluna))
      (setf (aref tabuleiro (1+ linha) coluna) nil))
    (incf linha)))

(defun tabuleiro-peca-explode (tabuleiro linha coluna)
  (cond ((tabuleiro-preenchido-p tabuleiro linha coluna)
    (tabuleiro-despreenche! tabuleiro linha coluna)
    1)
        (T 0)))

(defun calcula-linha-min-bomba (linha)
  (if (eql linha 0)
    0
    (1- linha)))

(defun calcula-linha-max-bomba (linha)
  (if (eql linha 17)
    17
    (1+ linha)))

(defun calcula-coluna-min-bomba (coluna)
  (if (eql coluna 0)
    0
    (1- coluna)))

(defun calcula-coluna-max-bomba (coluna)
  (if (eql coluna 9)
    9
    (1+ coluna)))

(defun calcula-pontos-bomba (explodidas)
  (* 5 (expt 2 (1- explodidas))))

(defun tabuleiro-remove-bomba! (tabuleiro linha coluna)
  (let ((linha-min (calcula-linha-min-bomba linha))
        (linha-max (calcula-linha-max-bomba linha))
        (coluna-min (calcula-coluna-min-bomba coluna))
        (coluna-max (calcula-coluna-max-bomba coluna))
        (explodidas 0))
    (loop for linha from linha-min to linha-max do
      (loop for coluna from coluna-min to coluna-max do
        (incf explodidas (tabuleiro-peca-explode tabuleiro linha coluna))))
    (calcula-pontos-bomba explodidas)))

(defun tabuleiro-topo-preenchido-p (tabuleiro)
  (dotimes (coluna 10)
    (if (tabuleiro-preenchido-p tabuleiro 17 coluna)
      (return T))))

(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
  (equalp tabuleiro1 tabuleiro2))

(defun tabuleiro->array (tabuleiro)
  (copia-tabuleiro tabuleiro))

(defun array->tabuleiro (tabuleiro)
  (copia-tabuleiro tabuleiro))


; *****************************************************************************
;                                  ESTADO
; *****************************************************************************

(defun cria-estado (pontos pecas-por-colocar pecas-colocadas tabuleiro)
  (make-estado :pontos pontos
               :pecas-por-colocar pecas-por-colocar
               :pecas-colocadas pecas-colocadas
               :tabuleiro tabuleiro))

(defun copia-estado (estado)
  (make-estado :pontos (estado-pontos estado)
               :pecas-por-colocar (copy-list (estado-pecas-por-colocar estado))
               :pecas-colocadas (copy-list (estado-pecas-colocadas estado))
               :tabuleiro (copia-tabuleiro (estado-tabuleiro estado))))

(defun estados-iguais-p (estado1 estado2)
  (and (eql (estado-pontos estado1) (estado-pontos estado2))
       (equal (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))
       (equal (estado-pecas-colocadas estado1) (estado-pecas-colocadas estado2))
       (tabuleiros-iguais-p (estado-tabuleiro estado1) (estado-tabuleiro estado2))))

(defun estado-final-p (estado)
  (or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
      (eql (estado-pecas-por-colocar estado) nil)))


; *****************************************************************************
;                                 PROBLEMA
; *****************************************************************************

(defun cria-problema (estado-inicial solucao accoes resultado custo-caminho)
  (make-problema :estado-inicial estado-inicial
                 :solucao solucao
                 :accoes accoes
                 :resultado resultado
                 :custo-caminho custo-caminho))

(defun solucao (estado)
  (and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))
       (eql (estado-pecas-por-colocar estado) nil)))

(defun rotacao-pecas (peca)
  (cond ((equal peca 'i) (list peca-i0 peca-i1))
        ((equal peca 'l) (list peca-l0 peca-l1 peca-l2 peca-l3))
        ((equal peca 'j) (list peca-j0 peca-j1 peca-j2 peca-j3))
        ((equal peca 'o) (list peca-o0))
        ((equal peca 's) (list peca-s0 peca-s1))
        ((equal peca 'z) (list peca-z0 peca-z1))
        ((equal peca 'bomba) (list peca-bomba))
        ((equal peca 't) (list peca-t0 peca-t1 peca-t2 peca-t3))
        ((equal peca 'u) (list peca-u0 peca-u1 peca-u2 peca-u3))
        (T (list))))

(defun accoes (estado)
  (let ((lista-pecas (rotacao-pecas (car (estado-pecas-por-colocar estado))))
       (lista-accoes '())
       (largura 0))
    (if (estado-final-p estado)
      (return-from accoes lista-accoes))
    (dolist (peca-actual lista-pecas)
      (setf largura (array-dimension peca-actual 1))
      (dotimes (i 10)
        (if (< (+ i largura) 11)
          (push (cria-accao i peca-actual) lista-accoes))))
    (reverse lista-accoes)))

(defun calcula-pontos-linhas (linhas)
  (cond ((eql linhas 0) 0)
        ((eql linhas 1) 100)
        ((eql linhas 2) 300)
        ((eql linhas 3) 500)
        (T 800)))

(defun peca-preenchida-p (peca linha coluna)
  (aref peca linha coluna))

(defun tabuleiro-calcula-linha (tabuleiro accao)
  (let* ((peca (cdr accao))
      (coluna (car accao))
      (largura-peca (array-dimension peca 1))
      (altura-peca (array-dimension peca 0))
      (altura-max 0)
      (linha 0)
      (diff +INFINITY+)
      (posicoes 0)
      (coluna-tabuleiro coluna))
    (dotimes (l largura-peca)
      (setf altura-max (max altura-max (tabuleiro-altura-coluna tabuleiro l))))
    (setf linha altura-max)
    (dotimes (coluna-peca largura-peca)
      (dotimes (linha-peca altura-peca)
        (cond ((peca-preenchida-p peca linha-peca coluna-peca) (return))
               (T (incf posicoes))))
      (setf diff (min diff (- (+ posicoes linha)
        (tabuleiro-altura-coluna tabuleiro coluna-tabuleiro))))
      (incf coluna-tabuleiro)
      (setf posicoes 0))
    (decf linha diff)
    linha))

(defun tabuleiro-coloca-peca (tabuleiro accao)
  (let ((peca (cdr accao))
        (coluna (car accao))
        (linha (tabuleiro-calcula-linha tabuleiro accao)))
    (if (> linha 17)
      (return-from tabuleiro-coloca-peca nil))
    (dotimes (i (array-dimension peca 0))
      (setf coluna (car accao))
      (dotimes (j (array-dimension peca 1))
        (when (peca-preenchida-p peca i j)
          (tabuleiro-preenche! tabuleiro linha coluna))
        (incf coluna))
      (incf linha))
    (decf linha)))

(defun peca-e-bomba-p (accao)
  (equalp (rest accao) peca-bomba))

(defun tabuleiro-remove-linhas (tabuleiro)
  (let ((removidas 0)
        (linha 17))
    (loop while (>= linha 0) do
      (when (tabuleiro-linha-completa-p tabuleiro linha)
        (tabuleiro-remove-linha! tabuleiro linha)
        (incf removidas))
      (decf linha))
    (calcula-pontos-linhas removidas)))

(defun tabuleiro-pode-duplicar-p (pecas-colocadas)
  (eql (mod (list-length pecas-colocadas) 2) 0))

(defun resultado (estado accao)
  (let* ((novo-estado (copia-estado estado))
        (peca (first (estado-pecas-por-colocar novo-estado)))
        (linha 0)
        (coluna (car accao)))
    (push peca (estado-pecas-colocadas novo-estado))
    (pop (estado-pecas-por-colocar novo-estado))
    (setf linha (tabuleiro-coloca-peca (estado-tabuleiro novo-estado) accao))
    (if (tabuleiro-topo-preenchido-p (estado-tabuleiro novo-estado))
      (return-from resultado novo-estado))
    (cond ((peca-e-bomba-p accao) (incf (estado-pontos novo-estado)
      (tabuleiro-remove-bomba! (estado-tabuleiro novo-estado) linha coluna)))
          (T (incf (estado-pontos novo-estado)
      (tabuleiro-remove-linhas (estado-tabuleiro novo-estado)))))
    (if (tabuleiro-pode-duplicar-p (estado-pecas-colocadas novo-estado))
      (setf (estado-tabuleiro novo-estado)
            (tabuleiro-linha-inferior-duplicada (estado-tabuleiro novo-estado))))
    novo-estado))

(defun qualidade (estado)
  (* (estado-pontos estado) -1))

(defun calcula-pontos-peca (peca)
  (cond ((equal peca 'i) 800)
        ((equal peca 'l) 500)
        ((equal peca 'j) 500)
        ((equal peca 'o) 300)
        ((equal peca 's) 300)
        ((equal peca 'z) 300)
        ((equal peca 'bomba) 640)
        ((equal peca 't) 300)
        ((equal peca 'u) 500)
        (T 0)))

(defun custo-oportunidade (estado)
  (let ((maximo-possivel 0))
    (dolist (peca (estado-pecas-colocadas estado))
      (incf maximo-possivel (calcula-pontos-peca peca)))
    (- maximo-possivel (estado-pontos estado))))

; *****************************************************************************
;                                 PROCURAS
; *****************************************************************************

;                      Procura em Profundidade Primeiro

(defun procura-pp (problema)
   (procura-pp-aux (problema-estado-inicial problema)
                   (problema-solucao problema)
                   (problema-accoes problema)
                   (problema-resultado problema)
                   (list)))

(defun procura-pp-aux (estado-inicial solucao? accoes! resultado! lista-accoes)
    (if (funcall solucao? estado-inicial)
      (return-from procura-pp-aux lista-accoes))
    (let ((accoes-disponiveis (reverse (funcall accoes! estado-inicial)))
          (novo-estado)
          (caminho nil))
      (dolist (accao accoes-disponiveis)
        (setf novo-estado (funcall resultado! estado-inicial accao))
        (setf caminho (procura-pp-aux novo-estado
                                      solucao?
                                      accoes!
                                      resultado!
                                      (append lista-accoes (list accao))))
          (if caminho (return-from procura-pp-aux caminho)))))


;                               Procura A*

(defun cria-no (estado accoes custo)
  (make-no :estado estado
           :accoes accoes
           :custo custo))

; Heap structure
; adapted from http://aima.cs.berkeley.edu/lisp/utilities/queue.lisp

(defun cria-heap ()
  (make-array 100 :fill-pointer 0 :adjustable t))

(defun heap-custo (heap i f)
  (declare (fixnum i))
  (funcall f (aref heap i)))

(defun heap-pai (i)
  (declare (fixnum i))
  (floor (1- i) 2))

(defun heap-esquerdo (i)
  (declare (fixnum i))
  (the fixnum (+ 1 i i)))

(defun heap-direito (i)
  (declare (fixnum i))
  (the fixnum (+ 2 i i)))

(defun heapify (heap i f)
  (let ((esquerdo (heap-esquerdo i))
        (direito (heap-direito i))
        (N (1- (length heap)))
        menor)
    (if (and (<= esquerdo N)
             (<= (heap-custo heap esquerdo f) (heap-custo heap i f)))
      (setf menor esquerdo)
      (setf menor i))
    (if (and (<= direito N)
             (<= (heap-custo heap direito f) (heap-custo heap menor f)))
      (setf menor direito))
    (when (/= menor i)
      (rotatef (aref heap i) (aref heap menor))
      (heapify heap menor f))))

(defun heap-pop-min (heap f)
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap 0 f)
    min))

(defun heap-insere (heap no f)
  (vector-push-extend nil heap)
  (let ((i (1- (length heap)))
        (custo (funcall f no)))
    (loop while (and (> i 0) (>= (heap-custo heap (heap-pai i) f) custo)) do
      (setf (aref heap i) (aref heap (heap-pai i)) i (heap-pai i)))
    (setf (aref heap i) no)
    heap))

(defun push-heap (heap no)
  (heap-insere heap no #'custo-no))

(defun pop-heap (heap)
  (if (not (zerop (fill-pointer heap)))
    (heap-pop-min heap #'custo-no)))

(defun custo-no (no)
  (no-custo no))

(defun calcula-custo (estado g h)
  (+ (funcall g estado)
     (funcall h estado)))

(defun procura-A* (problema heuristica)
  (procura-A*-aux (problema-estado-inicial problema)
                  (problema-solucao problema)
                  (problema-accoes problema)
                  (problema-resultado problema)
                  (problema-custo-caminho problema)
                  heuristica))

(defun procura-A*-aux (estado-inicial solucao? accoes! resultado! custo-caminho! heuristica)
  (let ((nos (cria-heap))
        (no-inicial (cria-no estado-inicial
                             nil
                             (calcula-custo estado-inicial custo-caminho! heuristica))))
    (setf nos (push-heap nos no-inicial))
    (loop while (not (zerop (fill-pointer nos))) do
      (let ((no (pop-heap nos)))
        (cond ((null no) (return nil))
              ((funcall solucao? (no-estado no))
               (return-from procura-A*-aux (no-accoes no)))
              (T
                (let* ((estado (no-estado no))
                       (lista-accoes (funcall accoes! estado)))
                  (dolist (accao lista-accoes)
                    (let* ((novo-estado (funcall resultado! estado accao))
                           (novo-no (cria-no novo-estado
                                             (append (no-accoes no) (list accao))
                                             (calcula-custo novo-estado
                                                            custo-caminho!
                                                            heuristica))))
                      (setf nos (push-heap nos novo-no)))))))))
    nil))


;                              Procura Best

(defun procura-best (array lista-pecas)
  (let* ((estado-inicial (cria-estado 0 lista-pecas (list) array))
         (problema (cria-problema estado-inicial
                                  #'solucao
                                  #'accoes
                                  #'resultado
                                  #'qualidade)))
    (procura-A* problema #'heuristica-combinada)))


;                             Heuristicas

; queremos minimizar
(defun heuristica-altura-agregada (estado)
  (let ((tabuleiro (estado-tabuleiro estado))
         (altura 0))
    (dotimes (coluna 10)
      (incf altura (tabuleiro-altura-coluna tabuleiro coluna)))
    altura))

; queremos maximizar
(defun heuristica-linhas-completas (estado)
  (let ((tabuleiro (estado-tabuleiro estado))
         (linhas 0))
    (dotimes (linha 18)
      (if (tabuleiro-linha-completa-p tabuleiro linha)
        (incf linhas)))
    linhas))

; queremos maximizar
(defun heuristica-pontos (estado)
  (/ (estado-pontos estado) 100))

; queremos minimizar
(defun heuristica-buracos (estado)
  (let ((tabuleiro (estado-tabuleiro estado))
        (buracos 0))
    (dotimes (coluna 10)
      (incf buracos (tabuleiro-coluna-buracos tabuleiro coluna)))
    buracos))

; queremos minimizar
(defun heuristica-desfasamento (estado)
  (let ((tabuleiro (estado-tabuleiro estado))
        (desfasamento 0))
    (dotimes (coluna 9)
      (incf desfasamento
        (abs (- (tabuleiro-altura-coluna tabuleiro coluna)
                (tabuleiro-altura-coluna tabuleiro (1+ coluna))))))
    desfasamento))


(defun heuristica-combinada (estado)
  (* 10 (+ (* 0.51066 (heuristica-altura-agregada estado))
           (* -0.76066 (heuristica-linhas-completas estado))
           (* -0.2 (heuristica-pontos estado))
           (* 0.35663 (heuristica-buracos estado))
           (* 0.184483 (heuristica-desfasamento estado))
           (* 0.04 (custo-oportunidade estado)))))


(load "utils.fas")
