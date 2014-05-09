% Estructuras de datos funcionales
% Juan Pablo Darago
% \today

# Persistencia funcional

## Introducción

* Hoy vamos a hablar de estructuras de datos funcionales.
* ¿Qué son?
    * En lenguajes funcionales no podemos mutar el estado.
    * Funciones matemáticas: Devuelven nuevos valores.
    * No queremos ser ineficientes.
* Nos va a interesar la persistencia totalmente funcional.

## Tipos de persistencia

* Cada cambio a una estructura funcional es una nueva _version_
* Persistencia parcial:
    * Todas las versiones son accesibles, solo la ultima es modificable.
* Persistencia total:
    * Todas las versiones son accesibles y modificables.
* Persistencia confluente:
    * Dos versiones se pueden combinar para generar otra.
* Persistencia funcional:
    * No se puede modificar un nodo, solo crear nuevos.

## Que nos interesa

* Vamos a trabajar con persistencia funcional.
* Resultados copados para los otros tipos: (MIT 6.851)
    * Estructuras del tipo _pointer-machine_ (_structs_ de C) pueden hacerse parciales
    con O(1) amortizado overhead y O(1) espacio por cambio.
    * Cotas parecidas para persistencia total.

## Conceptos

* Estructuras arboreas y path copying.
* Analisis amortizado.
* Evaluación lazy.

## Path copying

* Si tenemos una estructura que es un arbol, podemos compartir links.
* Ahorramos espacio y tiempo (no hace falta copiar todo).
* Ejemplo: lista simplemente enlazada y árboles.

## Lista simplemente enlazada

![Listas simplemente enlazada](images/lista-enlazada.pdf)

## Path copying

![Arbol con _path copying_](images/arbol-path-copying.pdf)

## Análisis amortizado:

* Muchas estructuras dan garantías para grupos de operaciones, no para cada una.
* Ejemplo: Vectores dinámicos.
    * $2^{n}$, $O(1)$ operaciones por cada operación $O(n)$
* Concepto: Análisis amortizado.
    * Definir un costo $a_i$ amortizado para cada operación i.
    * Probar que $\sum_{i = 1}^{j} a_i \geq \sum_{i = 1}^{j} t_i$ para $j \leq m$
    * Métodos: Del Banquero y del Físico.
* Se rompe con persistencia pero se puede arreglar.

## Ejemplo: Colas con listas simplemente enlazadas.

~~~haskell
data Queue = BQ [a] [a] 

check [] r = BQ (reverse r) []
check f r = BQ f r 

empty = BQ [] []

isEmpty (BQ f r) = null f

snoc (BQ f r) x = check f (x : r)

head (BQ [] _) = error "empty queue"
head (BQ (x : f) r ) = x

tail (BQ [] _) = error "empty queue"
tail (BQ (x : f) r) = check f r
~~~

## Persistencia

* La versión anterior se rompe con persistencia:
    * Que pasa si agarro la lista al borde de reversear y le hago tail?
* Para resolverlo, dos ideas:
    * Evaluación _lazy_: Gratis en Haskell, posible en SML, Scheme, etc.
    * Análisis amortizado: Reemplazar créditos con deuda.

## Ejemplo cola con persistencia.

~~~haskell
data Queue = BQ Int [a] Int [b]

check lenf f lenr r
    | lenf <= lenr = BQ lenf f lenr r
    | otherwise = BQ (lenf+lenr) (f ++ reverse r) 0 []

empty = BQ 0 [] 0 []

isEmpty (BQ lenf f lenr r) = (lenf == 0)

snoc (BQ lenf f lenr r) x = check lenf f (lenr+1) (x : r)

head (BQ lenf [] lenr r) x = error "empty queue"
head (BQ lenf (x : f') lenr r) = x

tail (BQ lenf [] lenr r) = error "empty queue"
tail (BQ lenf (x : f') lenr r) = check (lenf - 1) f' lenr r
~~~
## Otras ideas interesantes por Okasaki

* Estructuras basadas en el sistema de numeración.
    * Vamos a ver un par de ideas sobre esto.
* __Bootstrapping__
* __Lazy rebuilding__ 
* __Implicit Recursive Slowdown__

# Estructuras de datos
## Hash Array Mapped Trie - Vectores (Clojure)

* Creadas por Phil Bagwell en 2001.
    * Rich Hickey las adaptó para las estructuras de Clojure.
* La base es la idea de árboles.
* Usamos _path copying_ para persistencia.
* Nodos con un gran _branching factor_: 32, 64, etc.
    * Vamos a usar 4 en los ejemplos.
* En el caso de los vectores, representación densa.
    * Distinto a los maps.

## HAMT - Vector - Ejemplo

![HAMT sencillo con factor 4](images/array-indexed-trie-vector.pdf)

## HAMT - Vector - Ejemplo Inserción

![Inserción fácil en un HAMT tipo vector](images/array-indexed-trie-insert-easy.pdf)

## HAMT - Vector - Ejemplo Inserción

![Inserción más complicada en un HAMT tipo vector](images/array-indexed-trie-insert-harder.pdf)

## HAMT - Vector - Complejidades

* El principal uso es para un vector dinámico.
* _push_, _pop_, _update_ y _lookup_:
    * $O(\log n)$ pero la constante depende del _branch factor_.
    * con 32, es $O(\log_{32} n)$.
    * El $\log_{32} 10^{10} \approx 6$.
    * $O(1)$ de los pobres :).
* Las copias duelen bastante poco.
* Más feliz para la caché que una lista enlazada.
    * Aún así, hay que seguir punteros.

## Hash Array Mapped Trie - Optimizaciones en Clojure

* Principal optimización: Mantener un buffer (_tail_) de 32 valores.
    * Los últimos 32 valores los metemos ahi.
    * Convierte dos de los $O(\log n)$ en $O(1)$ muchas veces.
    * Cuando el buffer se llena lo insertamos de un saque en el vector.
* Posibles consideraciones para el 32.
    * En Java no tiene sentido (entero es de 32 bits).
    * Tamaño de caché.
    * Tamaño de palabra (cantidad de bits a shiftear).
    * Optimizaciones de cercania (JVM Heap compression).
* Pero sorprendentemente esto es muy rápido.
    * Operaciones de bits para seguir.

## Código de busqueda - Tomado de Clojure

~~~java
public Object[] arrayFor(int i){
    if(i >= 0 && i < cnt){
        if(i >= tailoff())
            return tail;

        Node node = root;
        for(int level = shift; level > 0; level -= 5)
            node = (Node) node.array[(i >>> level) & 0x01f];
        return node.array;
    }
    throw new IndexOutOfBoundsException();
}
~~~
## Otra implementación de Vectores - RRB Trees

* Agregar una operación interesante: _join_, _insertat_, _split_ en $O(\log n)$.
    * Util para workloads paralelos (_fork_ seguido de _join_).
* Relajar el _branch factor_ $m$.
    * No vale busqueda a lo trie, hijos pueden ser mas chicos.
    * Mantener lista de cantidad de hijos en los nodos.
    * Escaneo lineal: mejor comportamiento de caché.
    * Aumenta costo acceso.
* Invariante para concatenación: nodos tienen $m$ o $m-1$.
    * Mergear nodos _bottom up_.
* No queda claro si son una mejora (caso _index_ muy común).

## Hash Array Mapped Trie - Maps de Clojure

* Modificando esta misma estructura podemos hacer un diccionario.
    * HAMT usando un hash de la clave.
    * Base de _PersistentHashMap_
    * _PersistentTreeMap_ se basa en Red Black Trees.
* Esta vez sin embargo es necesaria una representación esparsa.
    * Muchos agujeros. Se pierde memoria.
* Nodos:
    * _Bitmap_ de que indices estan + Vector ordenado.
    * Posición: _and_ de con el bitmap y usar _popcount_.
    * Dos instrucciones en x86. Muy muy rápido.
    * El costo de tener que insertar se paga al copiar el nodo.

## Ejemplo de HAMT con esta variante.

![HAMT simple con compresión](images/hamt-comprimido.pdf)

## Ejemplo código - PersistentHashMap

~~~java 

public Object find(int shift, int hash, 
    Object key, Object notFound){

    int bit = bitpos(hash, shift);
    if((bitmap & bit) == 0)
        return notFound;
    int idx = Integer.bitCount(bitmap & (bit - 1));
    Object keyOrNull = array[2*idx];
    Object valOrNode = array[2*idx+1];
    if(keyOrNull == null)
        return ((INode) valOrNode).find(shift + 5, hash, 
                                        key, notFound);
    if(Util.equiv(key, keyOrNull))
        return valOrNode;
    return notFound;
}
~~~

## IntMap - Data.IntMap - Patricia trees

* Por Chris Okasaki y Andy Gill.
* Base de _Data.IntMap_ en Haskell.
* Diccionario de claves enteras.
    * _lookup_, _insert_, _delete_ y _update_ en $O(1)$.
    * _merge_ en $O(n + m)$.
* Estructura trie binario con path copying.
    * PATRICIA Trees.
    * Evitar caminos largos innecesarios.
    * Guardar mascaras prefijas y evitar hijos nulos.
* Buena performance.
    * _bit hacks_ son pocas instrucciones de ASM.
    * Mejor en merges que Red Black trees, peor en lookups.

## IntMap - Estructura ejemplo

![IntMap {1: x, 4: y, 5: z}](images/patricia-tree.pdf)

## IntMap - Signatura + lookup en Haskell

~~~haskell
data IntMap a = Nil
              | Tip Int a
              | Bin Int Int (IntMap a) (IntMap a) 

lookup :: IntMap a -> Nat -> Maybe a

lookup n Nil = Nothing

lookup n (Tip n val) = Just val
lookup n (Tip _ _) = Nothing

lookup n (Bin prefix bit t0 t1)
    | nomatch n prefix bit = prefix = Nothing
    | zero n bit = lookup n t0 
    | otherwise = lookup n t1
~~~

## IntMap - Insert 

~~~haskell
join :: Prefix -> IntMap a -> Prefix -> IntMap a -> IntMap a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

insert :: Key -> a -> IntMap a -> IntMap a
insert k x t
  = case t of
      Bin p m l r 
        | nomatch k p m -> join k (Tip k x) p t
        | zero k m      -> Bin p m (insert k x l) r
        | otherwise     -> Bin p m l (insert k x r)
      Tip ky _
        | k==ky         -> Tip k x
        | otherwise     -> join k (Tip k x) ky t
      Nil -> Tip k x
~~~

## IntMap - Merge

~~~haskell
union :: IntMap a -> IntMap a -> IntMap a
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (union l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (union t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (union t1 r2)

union (Tip k x) t = insert k x t
union t (Tip k x) = insertWith (\_ y -> y) k x t  -- right bias
union Nil t       = t
union t Nil       = t
~~~

## Finger Trees

* Mejorar 2-3 trees para dar una estructura para secuencias.
    * $O(1)$ amortizado _push_, _pop_, _queue_ y _enqueue_
    * $O(\log n)$ amortizado _concat_, _split_ y _append_.
* Funcionan con cualquier operación asociativa con identidad.
    * Se definen con un _measure_ y funciones asociadas.
    * Ejemplo: _search_ para buscar donde se rompe un predicado.
* Representación numérica.
* Utiliza fuertemente evaluación _lazy_.

## Finger Trees - Estructura

![Finger tree](images/finger-tree.pdf)

## Finger Trees - Ejemplo código

~~~haskell
data Digit a = One a | Two a a | Three a a a | Four a a a a

data FingerTree v a = Empty | Single a
	| Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)

instance (Monoid v) => Measured v (Node v a) where
    measure (Node2 v _ _)    =  v
    measure (Node3 v _ _ _)  =  v
~~~

## Finger Trees - Ejemplo inserción

~~~haskell
mV :: (M v a) => v -> FT v a -> v
mV v Empty = v
mV v t = v `mappend` measure t

consDigit :: a -> Digit a -> Digit a
consDigit a (One b) = Two a b
consDigit a (Two b c) = Three a b c
consDigit a (Three b c d) = Four a b c d

deep ::  (M v a) => D a -> FT v (Node v a) -> D a -> FT v a

deep pr m sf = Deep ((measure pr `mV` m) `><` measure sf) 
                    pr m sf

(<|) :: (M v a) => a -> FT v a -> FT v a
a <| Empty      =  Single a
a <| Single b       =  deep (One a) Empty (One b)
a <| Deep _ (Four b c d e) m sf = m `seq`
    deep (Two a b) (node3 c d e <| m) sf
a <| Deep _ pr m sf =  deep (consDigit a pr) m sf
~~~

* Abreviaturas por tamaño de slide.

## Purely functional real time deques with catenation

## Performance - Siempre tener en cuenta

Evento                Latencia
--------------------- -----------
1 ciclo CPU           0.3 ns
L1 Cache (32K)        0.9 ns
L2 Cache (256K)       1.8 ns
L3 Cache (8M)         12.9 ns
DRAM                  120 ns
SSD                   50-150 ns
HDD                   4ms

Tabla: Tiempos de acceso ("Systems Performance: Enterprise and the Cloud").

# Usos
## Multithreading - Consideraciones

* Multiples _threads_ + estado global = desastre.
* Necesario para aprovechar todos los cores.
* Dos opciones: O meter _locks_ o no tener estado global.
* Los _locks_ son muy molestos de usar.
* Los _locks_ son muy pesados.
    * Las variables globales tienen que pasar por las cachés.
    * Invalidan cachés de otros cores.

## Ejemplo arquitectura

![](images/system-arch.eps)

## Estructuras puramente funcionales

* ¡Se pueden compartir libremente!
* Resultados intermedios se pueden juntar rápido.
    * Por eso el interés en _split_ y _concat_.
* Se puede usar con _software transactional memory_
    * Mantener consistencia de los accesos.
    * Manera en que Clojure encara el problema.
    * Usando MVCC + Estructuras de datos persistentes se puede hacer rápido.
* No esta claro que el _overhead_ sea lo suficientemente bajo todavía.

## Planar point location using Persistent Search Trees

* Por Sarnak y Tarjan.
* _Problema_: Dados polígonos formados por la intersección de $n$ líneas, 
responder en que polígono esta una serie de puntos...rápido.
* Usar estructuras de datos persistentes.
    * _Parcialmente persistentes: Red Black Trees modificados.
    * Se puede hacer con RBT funcionales pero con más costo en memoria.

## ClojureScript + Om + ReactJS
