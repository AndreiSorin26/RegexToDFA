

class Nfa[A](stari: Set[A], alfabet: Set[Char], tranzitii: Set[(A, Char, A)], stareInitiala: A, stareFinala: A)
{
    def map[B](f: A => B) : Nfa[B] = new Nfa[B](stari.map(f), alfabet, tranzitii.map( t => (f(t._1), t._2, f(t._3)) ), f(stareInitiala), f(stareFinala))

    def next(state:A, c: Char): Set[A] = tranzitii.filter( t => t._1 == state && t._2 == c ).map(t => t._3)

    def isFinal(state: A): Boolean = state == stareFinala

    def epsilon(state: A): Set[A] =
        val aux = next(state, Nfa.EPS).diff(Set(state))
        aux.isEmpty match
            case true => Set(state)
            case _ => aux.foldLeft(Set[A]())((a: Set[A], s: A) => a.union(epsilon(s))).union(Set(state))

    def accepts(str: String): Boolean =
        def accepts_aux(str: String, stare: A): Boolean =
            str.isEmpty match
                case true =>
                    isFinal(stare) match
                        case true => true
                        case _ => epsilon(stare).diff(Set(stare)).foldLeft(false)( (acc, s) => acc || accepts_aux(str, s) )
                case _ =>
                    val mananc = next(stare, str.head).foldLeft(false)( (acc, s) => acc || accepts_aux(str.tail, s) )
                    val nuMananc = epsilon(stare).diff(Set(stare)).foldLeft(false)( (acc, s) => acc || accepts_aux(str, s) )
                    mananc || nuMananc
                    
        accepts_aux(str, stareInitiala)
        
    def getStates: Set[A] = stari
    def getAlfabet: Set[Char] = alfabet
    def getTranzitii: Set[(A, Char, A)] = tranzitii
    def getInit: A = stareInitiala
    def getFinala: A = stareFinala
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa
{
    val EPS: Char = '\u0000';

    def fromTokens(tokens: List[String]): Nfa[Int] =
        var tok = tokens
        def fromTokens_aux(): Nfa[Int] =
            val first = tok.head
            tok = tok.tail
            first match
                case "STAR" => Nfa.star(fromTokens_aux())
                case "CONCAT" => Nfa.concatenare(fromTokens_aux(), fromTokens_aux())
                case "UNION" => Nfa.uniune(fromTokens_aux(), fromTokens_aux())
                case "MAYBE" => Nfa.poate(fromTokens_aux())
                case "PLUS" => Nfa.plus(fromTokens_aux())
                case x => 
                    x match
                        case "SPATIU" => Nfa.nfaNou(' ')
                        case "eps" => Nfa.nfaNou(Nfa.EPS)
                        case "void" => Nfa.nfaVid()
                        case _ => Nfa.nfaNou(first.charAt(0))
        fromTokens_aux()

    def fromPrenex(str: String): Nfa[Int] =
        fromTokens(str.replaceAll("' '", "SPATIU").split(" ").toList)

    def nfaNou(ch: Char): Nfa[Int] = new Nfa[Int](Set(0,1), Set(Nfa.EPS, ch), Set((0, ch, 1)), 0, 1)

    def nfaVid(): Nfa[Int] = new Nfa[Int](Set(0,1), Set(Nfa.EPS), Set(), 0, 1)

    def concatenare(a: Nfa[Int], b: Nfa[Int]): Nfa[Int] =
        val altB: Nfa[Int] = b.map((stare: Int) => stare + a.getStates.size)
        new Nfa[Int]( a.getStates.union(altB.getStates), a.getAlfabet.union(b.getAlfabet),  a.getTranzitii.union(altB.getTranzitii).union(Set((a.getFinala, Nfa.EPS, altB.getInit))), a.getInit, altB.getFinala)

    def uniune(a: Nfa[Int], b: Nfa[Int]): Nfa[Int] =
        val altA = a.map( (stare: Int) => stare + 1 )
        val altB = b.map( (stare: Int) => stare + 1 + a.getStates.size )
        val stareFinala = a.getStates.size + b.getStates.size + 1
        val tranzitiiNoi = Set( (0, Nfa.EPS, altA.getInit), (0, Nfa.EPS, altB.getInit), (altA.getFinala, Nfa.EPS, stareFinala), (altB.getFinala, Nfa.EPS, stareFinala))
        new Nfa[Int](altA.getStates.union(altB.getStates).union(Set(0, stareFinala)), a.getAlfabet.union(b.getAlfabet), altA.getTranzitii.union(altB.getTranzitii).union(tranzitiiNoi), 0, stareFinala)

    def star(a: Nfa[Int]): Nfa[Int] =
        val altA = a.map( (stare: Int) => stare + 1 )
        val stareFinala = 1 + a.getStates.size
        val tranzitiiNoi = Set( (0, Nfa.EPS, altA.getInit), (0, Nfa.EPS, stareFinala), (altA.getFinala, Nfa.EPS, altA.getInit), (altA.getFinala, Nfa.EPS, stareFinala));
        new Nfa[Int]( altA.getStates.union(Set(0,stareFinala)), a.getAlfabet, altA.getTranzitii.union(tranzitiiNoi), 0, stareFinala )

    def poate(a: Nfa[Int]): Nfa[Int] = Nfa.uniune( Nfa.nfaNou(Nfa.EPS), a)

    def plus(a: Nfa[Int]): Nfa[Int] = Nfa.concatenare( a, Nfa.star(a) )
}