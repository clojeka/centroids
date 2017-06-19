(*) Como obter as partições para o k-Fold Cross Validation

Primeiro particione sua base de dados em pedaços de 10 pedaços de 10%
cada --- k = 10.  Na primeira iteração, o pedaço P(1) é usado como
teste e os pedaços P(2) .. P(10) são usados como treinamento.

(*) Como desenhar árvores

(require pict/tree-layout)
(require pict)
(define node (text "(2 2) (black 72)"))

(define tree1 (tree-layout #:pict node
                           (tree-edge (tree-layout #:pict node))
                           (tree-edge (tree-layout #:pict node))))

(define tree2 (tree-layout #:pict node
                           (tree-layout #:pict node)
                           (tree-edge tree1)))

(naive-layered tree2)

(*) O tree maker

Consideremos apenas uma classe binárias de valores E, D --- de
``esquerda'' e ``direita.''  O algoritmo constrói uma árvore de
decisão baseada em centróides de cada classe.  

Considere como raiz da árvore o centróide de toda a base.  O algoritmo
então anexa como filho esquerdo à raiz o centróide das tuplas que
sejam apenas da classe E.  Similarmente para a classe D.  Com maior
precisão:

Seja E, D as duas classes da base.

  * Particione a base em duas partições E, D.

  * Compute os centróides C(E) e C(D).

  * Insira-os como filhos da raiz de uma árvore.

  * Compute a lista L(E) composta dos elementos da base que estejam
    mais próximos de C(E) do que de C(D).  (Faça o mesmo com respeito
    L(D)).

  * Como filho de C(E), insira o resultado do algoritmo considerando
    como L(E) como base.  (Faça o mesmo com respeito a C(D).)

