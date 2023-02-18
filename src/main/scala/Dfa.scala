class Dfa[A] (stari: Set[A], alfabet: Set[Char], tranzitii: Set[(A, Char, A)], stareInitiala: A, stariFinale: Set[A], val SINK: A)
{
  // The following methods are only the methods directly called by the test suite. You can (and should) define more.
    def map[B](f: A => B) : Dfa[B] = new Dfa[B](stari.map(f), alfabet, tranzitii.map( t => (f(t._1), t._2, f(t._3)) ), f(stareInitiala), stariFinale.map(f), f(SINK))

    def next(state:A, c: Char): A = if !tranzitii.exists(t => t._1 == state && t._2 == c) then SINK else tranzitii.filter(t => t._1 == state && t._2 == c ).head._3

    def accepts(str: String): Boolean =
        def accepts_aux(str: String, stare: A): Boolean =
            stare match
                case SINK => false
                case _ =>
                    str.isEmpty match
                        case true => isFinal(stare)
                        case _ => accepts_aux(str.tail, next(stare, str.head))

        accepts_aux(str, stareInitiala)

    def isFinal(state: A): Boolean = stariFinale.contains(state)

    def getStates: Set[A] = stari

    def getAlfabet: Set[Char] = alfabet

    def getTranzitii: Set[(A, Char, A)] = tranzitii

    def getInit: A = stareInitiala

    def getFinala: Set[A] = stariFinale
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa
{
    def fromPrenex(str: String): Dfa[Int] =
        val nfa = Nfa.fromPrenex(str)
        Dfa.fromNFA(nfa)

    def fromNFA(nfa: Nfa[Int]): Dfa[Int] =
        def subSet(nfa: Nfa[Int]): Dfa[Set[Int]] =
            val multistareInit: Set[Int] = nfa.epsilon(nfa.getInit)
            var multistari: Set[Set[Int]] = Set(multistareInit)
            var tranzitii: Set[(Set[Int], Char, Set[Int])] = Set()

            def f(multistare: Set[Int], ch: Char): Set[Int] = multistare.flatMap(s => nfa.next(s, ch)).flatMap(s => nfa.epsilon(s))

            def subSet_aux(multistare: Set[Int]): Unit =
                multistare.isEmpty match
                    case true =>
                    case _ =>
                        val noiTranzitii = nfa.getAlfabet.diff(Set(Nfa.EPS)).map(ch => (multistare, ch, f(multistare, ch)))
                        val continue = noiTranzitii.filter(t => t._3.isEmpty || !multistari.contains(t._3))
                        noiTranzitii.foreach(t => multistari = multistari.union(Set(t._3)))
                        tranzitii = tranzitii.union(noiTranzitii)
                        continue.foreach(t => subSet_aux(t._3))

            subSet_aux(multistareInit)
            new Dfa[Set[Int]](multistari, nfa.getAlfabet.diff(Set(Nfa.EPS)), tranzitii, multistareInit, multistari.filter(ms => ms.exists(nfa.isFinal)), Set())

        def transform(stare: Set[Int]): Int =
            stare.isEmpty match
                case true => -1
                case _ => stare.toList.sorted.foldLeft(0)((acc, x) => acc*10 + x)

        val aux = subSet(nfa)
        aux.map(s => transform(s))
}
