# Scala Regular Expression Engine

## Project Overview: Regular Expression Engine

This project implements a regular expression engine from the ground up. Regular expressions, often called "regex," are powerful tools for pattern matching in text. You've likely encountered them in text editors, command-line tools, or when validating input fields (like ensuring an email address has the correct format).

**What does this project do?**

At its core, this project takes a regular expression pattern (e.g., `a*b`) and an input string (e.g., `aaab`) and determines if the string matches the pattern.

**Why was it implemented this way? The Journey from Regex to Match**

Directly matching complex regular expressions against text can be inefficient. This project follows a standard and well-established approach used in many real-world regex engines, involving a two-step conversion:

1.  **Regular Expression (Regex) to Non-deterministic Finite Automaton (NFA):**
    *   First, the input regex string is parsed and transformed into an NFA.
    *   An NFA is like a conceptual machine that can be in multiple states at once. It's relatively straightforward to build an NFA that directly corresponds to the structure of a regular expression. For example, the regex `a|b` (match 'a' or 'b') translates naturally into an NFA that has paths for both 'a' and 'b'.

2.  **Non-deterministic Finite Automaton (NFA) to Deterministic Finite Automaton (DFA):**
    *   Next, the NFA is converted into a DFA.
    *   A DFA is another conceptual machine, but unlike an NFA, for any given state and input character, there's only *one* possible next state. This determinism makes DFAs much faster for matching strings.
    *   The process of converting an NFA to a DFA (called "subset construction") might result in a DFA with more states than the NFA, but the speed gained during matching usually makes this trade-off worthwhile.

**Why this approach?**

*   **Ease of Construction:** Building an NFA from a regex (using algorithms like Thompson's construction) is algorithmically simpler and more direct than trying to build a DFA straight away.
*   **Efficiency in Matching:** DFAs are significantly more efficient for the actual task of checking if a string matches the pattern. Once the DFA is built, matching a string of length 'N' typically takes time proportional to 'N', which is very fast.
*   **Understanding Core Concepts:** Implementing this pipeline provides a deep understanding of fundamental computer science concepts: automata theory, parsing, and algorithm design.

**Simple Examples:**

*   **Regex:** `cat`
    *   **Matches:** "cat"
    *   **Doesn't Match:** "dog", "catch", "scatter"
*   **Regex:** `a*b` (zero or more 'a's followed by a 'b')
    *   **Matches:** "b", "ab", "aaab"
    *   **Doesn't Match:** "a", "ba", "acb"
*   **Regex:** `(com|org|net)` (either "com", "org", or "net")
    *   **Matches:** "com", "org"
    *   **Doesn't Match:** "edu", "io"

This project demonstrates the ability to translate a high-level pattern (the regular expression) into an efficient computational structure (the DFA) capable of performing the desired matching.

---

## Technical Details

This section dives into the technical implementation of the regular expression engine, detailing the components, algorithms, and data structures used. The project is implemented in Scala (version 3.2.1) and uses SBT for build management.

### Core Components

The engine is primarily composed of three main components: a Regex Parser, an NFA implementation, and a DFA implementation.

1.  **Regex Parser (`Regex.scala`, `TreeNode.scala`, `Operator.scala`, `Variable.scala`)**
    *   **Input Processing:** The input regular expression string is first preprocessed. This involves:
        *   Identifying and tokenizing operators (`*`, `|`, `+`, `?`), parentheses, and variables (characters, character classes like `[0-9]`, special sequences like `eps` for epsilon or `void`).
        *   Expanding character ranges (e.g., `[a-c]` becomes `(a|b|c)`).
        *   Handling quoted characters (e.g. `' '` for space).
        *   **Implicit Concatenation:** A crucial step is the insertion of explicit concatenation operators (`.`) where they are implied in standard regex notation (e.g., `ab` becomes `a.b`, `a(bc)` becomes `a.(b.c)`). This is handled in `preprocess2`.
    *   **Expression Tree:** The tokenized and preprocessed regex is then converted into an expression tree. This is achieved using a modified Shunting-yard-like algorithm (`expressionTree` method in `Regex.scala`).
        *   Operators and variables become nodes in the tree, respecting operator precedence (`*`, `+`, `?` have higher precedence than concatenation `.`, which has higher precedence than union `|`).
        *   Parentheses are used to control the order of operations.
        *   The `TreeNode` class represents nodes in this tree, which can hold either a `Variable` or an `Operator`.
    *   **Prenex Notation:** Finally, the expression tree is traversed (specifically, a pre-order traversal - Root, Left, Right, or "RSD" as named in the code) to produce a "prenex" (prefix) string representation of the regex (e.g., `CONCAT a STAR b`). This prenex string is the input for the NFA construction phase.

2.  **NFA (Non-deterministic Finite Automaton) (`Nfa.scala`)**
    *   **Representation:** An NFA is represented by the `Nfa` class, which stores:
        *   A set of states (`stari`: `Set[A]`).
        *   An alphabet (`alfabet`: `Set[Char]`).
        *   A set of transitions (`tranzitii`: `Set[(A, Char, A)]`), where each tuple is `(from_state, character, to_state)`. Epsilon transitions are represented using a special character `Nfa.EPS`.
        *   A single initial state (`stareInitiala`: `A`).
        *   A single final state (`stareFinala`: `A`).
    *   **Thompson's Construction:** The `Nfa` companion object implements Thompson's construction algorithm to build NFAs from the prenex representation of the regex. It provides functions for basic regex operations:
        *   `nfaNou(char)`: Creates an NFA for a single character.
        *   `nfaVid()`: Creates an NFA for the empty language (matches nothing).
        *   `concatenare(nfa1, nfa2)`: Concatenates two NFAs.
        *   `uniune(nfa1, nfa2)`: Creates the union of two NFAs.
        *   `star(nfa)`: Applies the Kleene star operation (zero or more repetitions).
        *   `poate(nfa)`: Implements the `?` operator (zero or one occurrence), typically by `uniune(nfaNou(Nfa.EPS), nfa)`.
        *   `plus(nfa)`: Implements the `+` operator (one or more occurrences), typically by `concatenare(a, star(a))`.
        The `fromTokens` (and by extension `fromPrenex`) method recursively calls these construction functions based on the operators in the prenex string.
    *   **Epsilon Closure:** The `epsilon(state)` method calculates the epsilon closure of a state: the set of all states reachable from the given state using only epsilon transitions. This is crucial for both NFA simulation (`accepts` method) and for the NFA to DFA conversion.
    *   **String Acceptance (`accepts`):** The `accepts` method determines if the NFA accepts a given input string. It explores possible paths through the NFA, considering epsilon transitions and character transitions.

3.  **DFA (Deterministic Finite Automaton) (`Dfa.scala`)**
    *   **Representation:** A DFA is represented by the `Dfa` class, storing:
        *   A set of states (`stari`: `Set[A]`).
        *   An alphabet (`alfabet`: `Set[Char]`).
        *   A set of transitions (`tranzitii`: `Set[(A, Char, A)]`). For a DFA, for each state and character, there is at most one transition.
        *   A single initial state (`stareInitiala`: `A`).
        *   A set of final states (`stariFinale`: `Set[A]`).
        *   A special `SINK` state: A non-final state to which transitions go if no other transition is defined for a given state and character. This ensures the DFA is complete (has a transition for every character from every state).
    *   **Subset Construction (`fromNFA` in `Dfa` object):** The core of the DFA module is the conversion from an NFA to a DFA using the subset construction algorithm.
        *   **DFA States from NFA State Sets:** Each state in the DFA corresponds to a *set* of NFA states.
        *   **Initial DFA State:** The initial state of the DFA is the epsilon closure of the NFA's initial state.
        *   **Transitions:** For a DFA state `S` (which is a set of NFA states) and a character `c` from the alphabet, the next DFA state is found by:
            1.  Taking the union of all NFA states reachable from any state in `S` by a transition on `c`.
            2.  Computing the epsilon closure of this resulting set of NFA states. This new set of NFA states becomes a new DFA state.
        *   **Final DFA States:** A DFA state is final if its corresponding set of NFA states contains at least one of the NFA's final states.
        *   **State Renaming:** The implementation maps these sets of NFA states (e.g., `Set(1,2,5)`) to simpler integer representations for the DFA states (e.g., `125` or a unique ID) for easier management. Empty sets of NFA states (which can occur if a transition leads nowhere) are mapped to a SINK state (represented as `-1` before mapping).
    *   **String Acceptance (`accepts`):** The `accepts` method for a DFA is simpler than for an NFA. It iterates through the input string, character by character, moving from one DFA state to the next according to the transition function. If, after consuming all characters, the DFA is in a final state, the string is accepted.

### Key Algorithms

*   **Regex Parsing & Expression Tree Construction:** A custom recursive descent parser combined with logic to handle operator precedence, effectively a variant of the Shunting-yard algorithm's principles for converting infix to an abstract syntax tree (AST).
*   **Thompson's Construction:** Used for building an NFA from a regular expression. This algorithm is compositional, meaning NFAs for complex expressions are built from NFAs of their subexpressions.
*   **Subset Construction:** The standard algorithm for converting an NFA to an equivalent DFA. This algorithm systematically explores the NFA states reachable by various input sequences, grouping them into DFA states.
*   **Epsilon Closure:** An essential part of both NFA simulation and the subset construction algorithm.

### Data Structures

*   `Set`: Extensively used for representing sets of states, alphabets, and transitions, leveraging Scala's immutable collections.
*   `List`: Used for processing tokens during parsing and for the prenex representation.
*   `mutable.Stack`: Used in the `expressionTree` method for managing operators and nodes during the Shunting-yard-like parsing process.
*   Custom Classes: `Nfa`, `Dfa`, `TreeNode`, `Operator`, `Variable` are the primary data structures defining the automata and regex components.

### Code Comments

The Scala code includes comments explaining the purpose of methods and complex logic sections, particularly within the NFA and DFA construction algorithms.
The project uses MUnit for testing (as indicated in `build.sbt`).
