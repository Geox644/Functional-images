module Deep where

import Shallow (Point, Region, Transformation)
import qualified Shallow as S

{-
    Deep embeddings pentru regiuni și transformări. Fiecare regiune
    și transformare este reprezentată sub forma unui arbore sintactic
    (abstract syntax tree, AST) ce descrie secvența de operații care contribuie
    la construcția acelei regiuni sau transformări. De exemplu, expresia
    (circles 2), unde circles a fost definită în etapa 1, ar produce acum
    un rezultat similar cu

    Union (Circle 2.0) (Transform (Translation 6.0 0.0) (Circle 2.0)).

    Pentru a obține acest efect, toate funcțiile din etapa 1 sunt reimplementate
    astfel încât să utilizeze direct constructorul de date potrivit al unui
    tip de date. De exemplu, funcția fromPoints *este* acum constructorul
    FromPoints.

    Primul avantaj major al reprezentării bazate pe AST-uri este posibilitatea
    interpretării acesteia în diverse moduri pentru a reconstitui semnificații
    concrete variate ale regiunilor și transformărilor, e.g. regiuni ca funcții
    caracteristice, și transformări ca funcții pe puncte, ca în etapa 1.
    Vom vedea și alte semnificații concrete în etapa 3.

    Al doilea mare avantaj îl constituie posibilitatea simplificării AST-ului
    înaintea interpretării lui într-o manieră specifică. Observați deja cum
    funcțiile combineTransformations și applyTransformation de mai jos, văzute
    ca smart constructors, recunosc anumite cazuri particulare și simplifică
    AST-ul încă de la construcție.
-}
data RegionAST
    = FromPoints [Point]
    | Rectangle Float Float
    | Circle Float
    | Complement RegionAST
    | Union RegionAST RegionAST
    | Intersection RegionAST RegionAST
    | Transform TransformationAST RegionAST
    deriving (Show, Eq)

data TransformationAST
    = Translation Float Float
    | Scaling Float
    | Combine [TransformationAST]
    deriving (Show, Eq)

fromPoints :: [Point] -> RegionAST
fromPoints = FromPoints

rectangle :: Float -> Float -> RegionAST
rectangle = Rectangle

circle :: Float -> RegionAST
circle = Circle

complement :: RegionAST -> RegionAST
complement = Complement

union :: RegionAST -> RegionAST -> RegionAST
union = Union

intersection :: RegionAST -> RegionAST -> RegionAST
intersection = Intersection

translation :: Float -> Float -> TransformationAST
translation = Translation

scaling :: Float -> TransformationAST
scaling = Scaling

{-
    Smart constructor: dacă lista de transformări este singleton, înlocuiește
    lista cu unica transformare din listă; altfel, utilizează constructorul
    de date Combine.
-}
combineTransformations :: [TransformationAST] -> TransformationAST
combineTransformations [t] = t
combineTransformations t = Combine t

{-
    Smart constructor: dacă se dorește aplicarea unei liste vide de transformări
    asupra unei regiuni, întoarce regiunea ca atare; altfel, utilizează
    constructorul de date Transform.
-}
applyTransformation :: TransformationAST -> RegionAST -> RegionAST
applyTransformation (Combine []) region = region
applyTransformation transformation region = Transform transformation region

{-
    *** TODO ***

    Implementați funcția toTransformation, care constituie o interpretare
    a AST-ului unei transformări (TransformationAST), în vederea recuperării
    reprezentării concrete din etapa 1, sub forma unei funcții cu tipul
    Transformation = (Point -> Point).

    Nu este necesar să implementați de la zero, ci puteți invoca direct
    funcțiile din etapa 1, prefixându-le cu "S." (observați la începutul acestui
    fișier linia "import qualified Shallow as S"). Mai precis, funcțiile
    neprefixate, e.g. translation, sunt cele din acest fișier, iar funcțiile
    prefixate, e.g. S.translation, sunt cele din modulul Shallow.
-}
toTransformation :: TransformationAST -> Transformation
toTransformation (Translation dx dy) = S.translation dx dy
toTransformation (Scaling s) = S.scaling s
toTransformation (Combine ts) = S.combineTransformations (map toTransformation ts)

{-
    *** TODO ***

    Implementați funcția toRegion, care constituie o interpretare a AST-ului
    unei regiuni (RegionAST), în vederea recuperării reprezentării concrete
    din etapa 1, sub forma unei funcții caracteristice cu tipul
    Region = (Point -> Bool).
    -  interpretează un arbore de sintaxă abstractă (AST) al unei regiuni (de tip RegionAST) și îl convertește în Region
-}
toRegion :: RegionAST -> Region
toRegion (FromPoints ps) = S.fromPoints ps
toRegion (Rectangle w h) = S.rectangle w h
toRegion (Circle r) = S.circle r
toRegion (Complement reg) = S.complement (toRegion reg)
toRegion (Union reg1 reg2) = S.union (toRegion reg1) (toRegion reg2)
toRegion (Intersection reg1 reg2) = S.intersection (toRegion reg1) (toRegion reg2)
toRegion (Transform trans reg) = S.applyTransformation (toTransformation trans) (toRegion reg)

{-
    Varianta actualizată a a funcției inside.
-}
inside :: Point -> RegionAST -> Bool
inside = flip toRegion

{-
    *** TODO ***

    Implementați funcția decomposeTransformation, care descompune o transformare
    oricât de complexă într-o listă de transformări elementare (translații
    și scalări), conservând bineînțeles ordinea acestora.

    Constrângeri: evitați recursivitatea explicită.

    Hint: valorificați ordinea de sus în jos de realizare a pattern matching-ului,
    pentru a scurta descrierea cazurilor.

    Exemple:

    > decomposeTransformation $ Translation 1 2
    [Translation 1.0 2.0]

    > decomposeTransformation $ Scaling 2
    [Scaling 2.0]

    > decomposeTransformation $
        Combine [ Translation 1 2
                , Combine [ Translation 3 4
                          , Scaling 2
                          ]
                , Scaling 3
                ]
    [Translation 1.0 2.0,Translation 3.0 4.0,Scaling 2.0,Scaling 3.0]
descompune o transformare complexă într-o listă de transformări elementare
concatMap aplică funcția decomposeTransformation fiecărei transformări din listă și concatenează rezultatele într-o singură listă
-}

decomposeTransformation :: TransformationAST -> [TransformationAST]
decomposeTransformation (Translation dx dy) = [Translation dx dy]
decomposeTransformation (Scaling s) = [Scaling s]
decomposeTransformation (Combine ts) = concatMap decomposeTransformation ts

{-
    *** TODO ***

    Implementați funcția fuseTransformations, care alipește transformările
    adiacente de același fel (translații cu translații și scalări cu scalări)
    dintr-o listă de transformări elementare (translații și scalări),
    și întoarce lista transformărilor rezultante.

    Constrângeri: evitați recursivitatea explicită.

    > fuseTransformations [Translation 1 2]
    [Translation 1.0 2.0]

    > fuseTransformations [Scaling 2, Scaling 3]             
    [Scaling 6.0]

    > fuseTransformations [ Translation 1 2, Translation 3 4
                          , Scaling 2, Scaling 3
                          , Translation 5 6
                          ]
    [Translation 4.0 6.0,Scaling 6.0,Translation 5.0 6.0]
-  combină transformările adiacente de același tip
- foldr este folosit pentru a parcurge lista de transformări
 de la dreapta la stânga, cu un acumulator de rezultat.
 - combina transformările adiacente de același tip într-o listă de transformări simplificate.
-}

fuseTransformations :: [TransformationAST] -> [TransformationAST]
fuseTransformations = foldr fuseAux []
  where
    fuseAux (Translation dx1 dy1) (Translation dx2 dy2 : ts) = Translation (dx1 + dx2) (dy1 + dy2) : ts
    fuseAux (Scaling s1) (Scaling s2 : ts) = Scaling (s1 * s2) : ts
    fuseAux t ts = t : ts
-- Dacă următoarea transformare nu este de același tip,
-- se păstrează transformarea actuală și se continuă cu lista de transformări restante.


{-
    *** TODO ***

    Implementați funcția optimizeTransformations, care simplifică toate
    transformările din AST-ul unei regiuni. Principiile sunt următoarele:

    * Transformările succesive trebuie descompuse și alipite.
    * Pentru a evidenția lanțuri cât mai lungi de transformări succesive,
      se urmărește deplasarea în sus a transformărilor din AST, astfel:
      * Complementul unei transformări este transformarea complementului.
      * O ramificare (reuniune sau intersecție) de transformări de regiuni
        presupune determinarea celui mai lung prefix de transformări comune
        de pe cele două ramuri și deplasarea acestuia deasupra ramificării,
        păstrând sub ea sufixele de transformări necomune.
    * O regiune elementară (din puncte, dreptunghi sau cerc) se consideră
      optimizată.
    * Toate cosmetizările de mai sus se realizează după optimizarea recursivă
      a subregiunilor.
    
    Constrângeri: evitați duplicarea codului.

    Hints:

    * case pentru pattern matching în interiorul altor expresii
    * smart constructors: combineTransformation, applyTransformation

    Exemple:

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Circle 5)
    Transform (Combine [Translation 4.0 6.0,Scaling 6.0]) (Circle 5.0)

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Transform (Scaling 4)
                             (Transform (Scaling 2) (Circle 5)))
    Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Circle 5.0)

    > optimizeTransformations $
        Complement (Transform (Scaling 4)
                              (Transform (Scaling 2) (Circle 5)))
    Transform (Scaling 8.0) (Complement (Circle 5.0))

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Complement (Transform (Scaling 4)
                                         (Transform (Scaling 2) (Circle 5))))
    Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Complement (Circle 5.0))

    > optimizeTransformations $
        Union (Complement (Transform (Scaling 4)
                                     (Transform (Scaling 2) (Circle 5))))
              (Rectangle 6 7)
    Union (Transform (Scaling 8.0) (Complement (Circle 5.0))) (Rectangle 6.0 7.0)

    > optimizeTransformations $
        Union (Transform (Combine [ Translation 1 2
                                  , Combine [ Translation 3 4
                                            , Scaling 2
                                            ]  
                                  , Scaling 3
                                  ])
                         (Complement (Transform (Scaling 4)
                                                (Transform (Scaling 2) (Circle 5)))))
              (Transform (Translation 4 6) (Rectangle 6 7))
    Transform (Translation 4.0 6.0)
              (Union (Transform (Scaling 48.0) (Complement (Circle 5.0)))
                     (Rectangle 6.0 7.0))
 simplifică transformările dintr-un AST de regiune, aplicând reguli de combinare și descompunere.
 utilizez pattern matching pentru a diferentia diferitele tipuri de noduri din AST-ul de regiuni 
-- daca subregiunea optimizata este inca o transformare, combina transformarea curenta cu transformarea interioara
si aplic transformarile combinate
-}

optimizeTransformations :: RegionAST -> RegionAST
optimizeTransformations (Circle r) = Circle r
optimizeTransformations (Rectangle w h) = Rectangle w h
optimizeTransformations (FromPoints ps) = FromPoints ps

optimizeTransformations (Transform trans reg) =
    let optimizedReg = optimizeTransformations reg
    in case optimizedReg of 
        Transform innerTrans subReg ->
            let combinedTrans = combineTransformations [trans, innerTrans]
                fusedTrans = fuseTransformations (decomposeTransformation combinedTrans)
            in Transform (combineTransformations fusedTrans) subReg
            -- daca subregiunea nu este o transformare, se aplica functia doar pe transformarea initiala
        _ -> let fusedTrans = fuseTransformations (decomposeTransformation trans)
             in applyTransformation (combineTransformations fusedTrans) optimizedReg
-- transformările combinate sunt descompuse combinate in transformările adiacente de același tip
optimizeTransformations (Complement reg) =
    let optimizedReg = optimizeTransformations reg
    in case optimizedReg of
        -- Daca subregiunea optimizată este o transformare,
        -- aplic transformarea în afara complementului, daca nu aplic direct complementul
        Transform trans subReg -> Transform trans (Complement subReg)
        _ -> Complement optimizedReg

optimizeTransformations (Union r1 r2) = optimize Union r1 r2

optimizeTransformations (Intersection r1 r2) = optimize Intersection r1 r2

optimize :: (RegionAST -> RegionAST -> RegionAST) -> RegionAST -> RegionAST -> RegionAST
optimize constructor r1 r2 =
    let optimizedR1 = optimizeTransformations r1
        optimizedR2 = optimizeTransformations r2
    in constructor optimizedR1 optimizedR2

