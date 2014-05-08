Temas de la charla
==================

* Conceptos
    * Tipos de persistencia (<http://courses.csail.mit.edu/6.851/spring12/>)
        * Parcial, Completa, Confluente.
        * Resultados conocidos para ellas.
    * Conceptos para full persistance: (Libro de Okasaki)
        * Path copying: Listas simplemente enlazadas.
        * Analisis amortizado: Colas con listas simple enlazadas.
        * Agregando persistencia con evaluación lazy.
        * Analisis de estructuras amortizadas lazy:
            * Bankers Queue.
            * Physicist Queue.
        * Conceptos varios de diseño:
            * Scheduling.
            * Lazy rebuilding.
            * Bootstrapping.
* Estructuras:
    * Vectores persistentes de Clojure 
        * <http://lampwww.epfl.ch/papers/idealhashtrees.pdf>
        * <http://hypirion.com/musings/understanding-persistent-vector-pt-1>
        * Otra manera: <http://infoscience.epfl.ch/record/169879/files/RMTrees.pdf>
    * Purely functional worst case constant time catenable sorted lists.
        * <http://www.cs.au.dk/~gerth/papers/esa06trees.pdf>
    * IntMap
        * <http://ittc.ku.edu/~andygill/papers/IntMap98.pdf>
    * Finger Trees
        * <http://www.soi.city.ac.uk/~ross/papers/FingerTree.pdf>
    * Cosas generales: Zippers
        * <https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf>
* Usos:
    * Multithreading.
    * Volver al pasado a lo Braid.
    * ReactJS + ClojureScript + Om (<http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/>).
