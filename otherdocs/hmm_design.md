HMM Design
==========

A hidden Markov model is a graph, some vertices of which have associated
emission probability tables, and whose edges have associated transition
probabilities. For convenience, vertices are grouped into triples, which
we call _nodes_ (note that this is atypical graph language; in graph theory
_nodes_ and _edges_ are normally interchangeable concepts.)

Each node contains three vertices, each corresponding to a different
_state label_, representing the fact that a node can correspond to a
_match_ state, a _delete_ state, or an _insert_ state. The insert state
is special in that it has a self-edge, and can represent a cycle; it is
also special in that the match state has an edge to the insert state; a
node can be in the match state _and_ the insert state (this corresponds
to a query sequence having extra residues between one node and the next)

The zeroth node in an HMM is special; its match state does not emit.
Thus, its match state corresponds to the non-emitting _begin_ state, which
is occupied with probability 1. This begin state can transition to node 0's
insert state, which represents the query sequence containing extra residues
before the first node of the model. The begin state can also transition to the
_match_ or _delete_ states of node 1.

The nth node in an HMM is also special: it must transition to the _match_ state
of a non-existent _end_ node. Thus, the only transition probabilities that are
considered for the nth node are _match-to-insert_ and _insert-to-insert_ (in
case the query sequence contains additional residues after the last node of the
model), _insert-to-match_ (which transitions to the end), _match-to-match_
(which also transitions to the end) and _delete-to-match_ (which again
transitions to the end).

In the HMM+ file format, node 0 is treated differently (it contains no line
for match emissions, and certain of its transitions are represented as '*').
Likewise, node n is treated differently; its _delete-to-match_ and
_delete-to-delete_ transitions are represented as '*'.

However, in MRFy, there is a discordance between this legacy file format and
the hidden Markov model implementation. The beta strands force their
corresponding nodes into _match_ states, and the Viterbi algorithm is not
run on these beta-strand nodes. However, the initial node of a beta strand
corresponds to the illusory _end_ node for the previous hidden Markov model,
and the last node of a beta strand corresponds to the 0th (or _begin_) node
of the next hidden Markov model, so its transitions must be considered, as
well as its insertion emissions table.

As of July 9, 2012, MRFy treats the HMM read from disk as a vector of HMMNode
entries, and slices that vector to correspond to the beta strands. However,
it would be possible to transform the HMM into a list of smaller HMMs,
corresponding to the non-beta-strand parts of the model, and a list of
beta-strand scoring tables (for evaluating the HMM component of the
MRF score for the beta strands).

Also as of July 9, 2012, MRFy only performs _global_ alignments, which means
that every node of the model, and every residue of the query sequence, are
forced into correspondence. We wish to implement a _local_ alignment. In the
HMMER/Plan7 model, additional edges are placed between node 0's non-emitting
_match_ state (which is really _begin_) and every other node's _match_ state.
These are _begin-to-match_ transitions, and provide a means to short-cut
part of the model. Similarly, every node's _match_ state gains an edge to
the non-existent _end_ node; these are _match-to-end_ transitions. The
probabilities assigned to these edges are _not_ well documented, and thus
far we have attempted to reverse-engineer them.

Just as _begin-to-match_ and _match-to-end_ transitions make the _model_
local (called 'glocal' in the HMMER documentation), we also need to
be able to make the query sequence local. This is achieved by adding
two additional states, called _C_ (the C-terminal unaligned sequence
state) and _N_ (the N-terminal unaligned sequence state). This
terminology comes from the fact that an amino acid sequence has a direction,
determined by its C-terminal (as in carbon) and N-terminal (as in nitrogen)
ends. The _N_ state lives just before node 0, and has a self-edge as well
as an edge to node 0's _match_ state. Similarly, the _C_ state lives just
after the non-existent _end_ node. Both _N_ and _C_ have self-edges and
emit residues according to a background distribution.

In MRFy, we wish for the placement of beta strands to occur before any
consideration is paid to these local properties; in other words, only
the left-most HMM segment will consider left-side locality (_begin-to-match_
and the _N_ state), and only the right-most HMM segment will consider
right-side locality (_match-to-end_ and the _C_ state). Intermediate
HMMs, which are bounded on both sides by beta strands, are subject to
_global_ alignment only.

