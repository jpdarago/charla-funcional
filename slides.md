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

## Ejemplos

* Para ver esto, vamos a usar solo listas enlazadas.
    * Disponibles en cualquier lenguaje funcional.
    * Fáciles de persistir como ya vimos.
* Pilas: Trivial.
* Colas: No tan trivial.
    * Implementación de _snoc_ requiere recorrer toda la lista.

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

* Aunque el codigo de Haskell anterior anda...
* La versión anterior se rompe con persistencia:
    * Que pasa si agarro la lista al borde de reversear y le hago tail?
* Para resolverlo, dos ideas:
    * Evaluación _lazy_: Gratis en Haskell, posible en SML, Scheme, etc.
    * Análisis amortizado: Reemplazar créditos con deuda.

## Streams!

* En Haskell las listas ya son lazy.
* En Clojure hay tipos de datos que son lazy (seqs).
* En otros lenguajes se puede implementar.
    * Lo unico necesario es _side-effects_ y closures.
    * La interfaz mas linda requiere algo de syntax sugar.
    * "Maybe time is an illusion. Maybe nothing ever changes" - SICP.
* Son la forma más básica de suspensión de evaluación.

## Streams! En Scheme!

~~~scheme
(define (memo proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr) (memo (lambda () (expr))))))

(define (force delayed) (delayed))

(define (stream-cons a b) (cons a (delay b)))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
~~~

## Adaptando el banquero

* Ahora, no molesta "pagar" lo mismo dos veces:
    * El resultado de una misma suspensión queda guardado.
* _unshared cost_: Costo asumiendo todo memoizado.
* Puedo crear deuda (_debit_) 
    * Disjuntos en operaciones.
    * Creado por el costo de una suspensión.
    * Debe haber sido _descargado_ antes de usado.
* Cada operación puede ocuparse de pagar un poco.
    * _amortizado_ = _unshared_ + deuda pagada.
* Solo consideramos las suspensiones que se evaluan.

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
head (BQ lenf (x : ff) lenr r) = x

tail (BQ lenf [] lenr r) = error "empty queue"
tail (BQ lenf (x : ff) lenr r) = check (lenf - 1) ff lenr r
~~~

## _Sketch_ de demo usando banquero.

* _unshared cost_ es $O(1)$ por inspección.
* QVQ $O(1)$ descargar _debits_ alcanza para pagar las suspensiones.
* Sea $d(i)$ los _debits_ en el nodo $i$ del _stream_ $f$.
* $D(i) = \sum_{j = 0}^{i} d(j)$.
* Definimos el invariante $D(i) \leq \min (2i, \| f \| - \| r \|)$.
    * El término $2i$ es para que el primero este siempre pagado para
      cuando lo tengamos que acceder.
    * El término $\| f \| - \| r \|$ es para haber descargado todos
      los _debits_ antes de la rotación.
* La idea es probar que con $O(1)$ descargas de _debit_, _snoc_ y
  _tail_ mantienen el invariante.

## Otro ejemplo -- Hood Melville Queues

* A veces la amortización no nos sirve.
* Podemos hacer el append de reverse de a pasos
* Estrategia para Hood Melville queues.
    * $f ++ reverse(r)$ en 3 pasos: Rotar f y r, rotar de vuelta f en r.
    * Hacer dos pasos por cada operación.
    * Una lista para permitir _tail_ y _head_, otra para _snoc_
    * Contador para ir viendo cuantos elementos meter.

## Hood Melville queues - Invariante
~~~haskell
data RotationState a = 
    Idle 
    | Rev Int [a] [a] [a] [a] 
    | App Int [a] [a] 
    | Done [a]
data HoodMelvilleQueue a = HM Int [a] (RotationState a) Int [a]

exec (Rev ok (x:f) ff (y:r) rr) = Rev (ok+1) f (x:ff) r (y:rr)
exec (Rev ok [] ff [y] rr) = App ok ff (y:rr)
exec (App 0 ff rr) = Done rr
exec (App ok (x:ff) rr) = App (ok-1) ff (x:rr)
exec state = state

invalidate (Rev ok f ff r rr) = Rev (ok-1) f ff r rr
invalidate (App 0 ff (x:rr)) = Done rr
invalidate (App ok ff rr) = App (ok-1) ff rr
invalidate state = state
~~~

## Hood Melville Queue - Código

~~~haskell
exec2 lenf f state lenr r =
    case exec (exec state) of
        Done newf -> HM lenf newf Idle lenr r
        newstate -> HM lenf f newstate lenr r

check lenf f state lenr r = 
    if lenr <= lenf then exec2 lenf f state lenr
    else let newstate = Reversing 0 f [] r []
        in exec2 (lenf + lenr) f newstate 0 []

snoc (HM lenf f state lenr r) = 
    check lenf f state (lenr+1) (x:r)

head (HM _ (x:ff) _ _ _) = x

tail (HM lenf (x:ff) state lenr r) = 
    check (lenf-1) ff (invalidate state) lenr r
~~~

## Otras ideas interesantes por Okasaki

* Estructuras basadas en el sistema de numeración.
    * Base en parte de los _finger trees_.
* __Scheduling__
    * Pasar de amortizado a worst case.
    * Hood-Melville Queue.
* __Bootstrapping__
    * Usado en _finger trees_.
* __Lazy rebuilding__ 
    * Tambien usado para Hood-Melville Queue.
* __Implicit Recursive Slowdown__
    * Usado en _catenable deques_.
    * Base en parte de los _finger trees_.

# Estructuras de datos
## Introducción

* Algunas de las estructuras de Okasaki no son muy usadas en la práctica.
* A continuación vamos a ver algunas que si.
* Librerías estándares de
    * Haskell: Data.Sequence, Data.IntMap.
    * Clojure: Lisp Funcional sobre la JVM. 
        * Estructuras core del lenguaje son puramente funcionales.
    * Scala: Lenguaje funcional sobre la JVM.
        * Junta OOP, Functional y Reactive.
        * Tiene librerías similares a Haskell.

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
    * Solo copio un camino (máximo 7 nodos en un mundo feliz).
* Direccionar es barato: bit masking.
* Más feliz para la caché que una lista enlazada.
    * Aún así, hay que seguir punteros.
    * La JVM justo maneja eso bien.

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
    * Posición: _and_ con el bitmap y usar _popcount_.
    * Dos instrucciones en x86. Muy muy rápido.
    * El costo de tener que insertar se paga al copiar el nodo.

## Ejemplo de HAMT con esta variante.

![HAMT simple con compresión](images/hamt-comprimido.pdf)

## Ejemplo código - PersistentHashMap

~~~java 
static int mask(int hash, int shift){
	return (hash >>> shift) & 0x01f;
}
private static int bitpos(int hash, int shift){
	return 1 << mask(hash, shift);
}
public Object find(int shift, int hash, Object key, Object notF){
    int bit = bitpos(hash, shift);
    if((bitmap & bit) == 0)
        return notF;
    int idx = Integer.bitCount(bitmap & (bit - 1));
    Object keyOrNull = array[2*idx];
    Object valOrNode = array[2*idx+1];
    if(keyOrNull == null)
        return ((INode) valOrNode).find(shift + 5, hash, 
                                        key, notF);
    if(Util.equiv(key, keyOrNull))
        return valOrNode;
    return notF;
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
    | nomatch n prefix bit = Nothing
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
    * _search_ para buscar donde se rompe un predicado.
    * Ejemplo: Usar cantidad de hijos para indexar, 
    * Ejemplo: Mínimo de los valores para cola de prioridad.
* Utiliza fuertemente evaluación _lazy_.
* Requiere de una extensión de tipos en Haskell.

## Finger Trees - Estructura

![Finger tree](images/finger-tree.pdf)

## Finger Trees - Ejemplo código

~~~haskell
data Node a = Node2 Int a a | Node3 Int a a a
data Digit a = One a | Two a a | Three a a a | Four a a a a

data FingerTree v a = Empty | Single a
	| Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)

instance (Monoid v) => Measured v (Node v a) where
    measure (Node2 v _ _)    =  v
    measure (Node3 v _ _ _)  =  v

consDigit :: a -> Digit a -> Digit a
consDigit a (One b) = Two a b
consDigit a (Two b c) = Three a b c
consDigit a (Three b c d) = Four a b c d
~~~

## Finger Trees - Ejemplo inserción

~~~haskell
mV :: (M v a) => v -> FT v a -> v
mV v Empty = v
mV v t = v `mappend` measure t

deep ::  (M v a) => D a -> FT v (Node v a) -> D a -> FT v a
deep pr m sf = Deep ((measure pr `mV` m) `mappend` measure sf) 
                    pr m sf

(<|) :: (M v a) => a -> FT v a -> FT v a
a <| Empty      =  Single a
a <| Single b       =  deep (One a) Empty (One b)
a <| Deep _ (Four b c d e) m sf = m `seq`
    deep (Two a b) (node3 c d e <| m) sf
a <| Deep _ pr m sf =  deep (consDigit a pr) m sf
~~~

## Finger Trees - Ejemplo concatenación (version listas)

~~~haskell
nodes [a,b] = [Node2 a b]
nodes [a,b,c] = [Node3 a b c]
nodes [a,b,c,d] = [Node2 a b, Node2 c d]
nodes (a:b:c:xs) = (Node3 a b c):(nodes xs)

app3 Empty ts xs = ts <|| xs
app3 xs ts Empty = xs ||> ts
app3 (Single x) ts xs = x <| (ts <|| xs)
app3 xs ts (Single x) = (xs ||> ts) |> x
app3 (Deep pr1 m sf1) ts (Deep pr2 m2 sf2) =
    Deep pr1 (app3 m1 (nodes (sf1 ++ ts ++ pr2)) m2) sf2

concat :: FT a -> FT a -> FT a
concat xs ys = app3 xs ys
~~~

## Ejemplo - vistas de la estructura 

~~~Haskell
toTree s = s <|| Empty

data View s a = Nil | Cons a (s a)

view :: FT a -> V FT a
view Empty = Nil 
view (Single x) = Cons x Empty
view (Deep pr m sf) = Cons (head pr) (deepl (tail pr) m sf)

deepl :: FT a -> View FT a
deepl [] m sf = case viewL m of
                    Nil -> toTree sf
                    Cons a mm -> Deep (toList a) mm sf

deepl :: [a] -> FT (Node a) -> Digit a -> FT a
deepl pr m sf = Deep pr m sf

head x = case view x of Cons a _ -> a
tail x = case view x of Cons _ xx -> xx
~~~

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

Tabla de "Systems Performance: Enterprise and the Cloud"

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
    * La coordenada _x_ es el tiempo. Respondemos consultas de sucesor y 
      predecesor para árboles guardados.
    * _Parcialmente persistentes: Red Black Trees modificados_.
    * Se puede hacer con RBT funcionales pero con más costo en memoria.

## ClojureScript + Om + ReactJS

* Dos ingredientes:
* ReactJS
    * _Framework_ para hacer aplicaciones web en Javascript.
    * Algoritmo de _tree diffing_: DOM virtual contra DOM real.
        * Minima cantidad de cambios. Mejor que $O(n^3)$.
        * Heurísticas: Utilizar componentes para la vista.
* ClojureScript 
    * Estructuras de datos puramente funcionales.
    * Se llevan bien con la heurística de ReactJS.
* Resultado: Om
    * Estructuras puramente funcionales, igualdad tiene sentido.
    * El algoritmo de _diffing_ eficiente. _undo_ es trivial.

## ReactJS en Javascript 

~~~javascript
var DOM = React.DOM;
var Comment = React.createClass({
    render: function() {
        return React.DOM.div({className: "comment"},
            DOM.h2({className: "cAuth"}, this.props.author),
            DOM.span(null, this.props.text)
        );
    }
});
var CommentList = React.createClass({
    render: function() {
        var nodes = this.props.comments.map(function(comment){
            return Comment({author: comment.author,
                            text: comment.text });
        })
        return React.DOM.div({className: "cList"}, nodes);
    }
});
~~~

## Eso es todo

¿Preguntas?
