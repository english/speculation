module Speculation
  module Specs
    using Speculation::NamespacedSymbols.refine(self)

    S = Speculation::Core
    U = Speculation::Utils

    S.fdef(U.method(:hash?),
           args: S.tuple(:any.ns(S)),
           ret: :boolean.ns(S))

    S.fdef(U.method(:array?),
           args: S.tuple(:any.ns(S)),
           ret: :boolean.ns(S))

    S.fdef(U.method(:collection?),
           args: S.tuple(:any.ns(S)),
           ret: :boolean.ns(S))

    S.fdef(U.method(:identity),
           args: S.cat(x: :any.ns(S)),
           ret: :any.ns(S),
           fn: -> (x) { x[:args][:x].equal?(x[:ret]) })

    S.fdef(U.method(:constantly),
           args: S.cat(x: :any.ns(S)),
           ret: Proc,
           fn: -> (x) { x[:args][:x].equal?(x[:ret].call) })

    S.def(:coll.ns, -> (x) { U.collection?(x) })
    S.fdef(U.method(:distinct?),
           args: S.cat(coll: -> (coll) { U.collection?(coll) }),
           ret: :boolean.ns(S))

    S.fdef(U.method(:ident?),
           args: S.cat(x: :any.ns(S)),
           ret: :boolean.ns(S))

    S.fdef(U.method(:method?),
           args: S.cat(x: :any.ns(S)),
           ret: :boolean.ns(S))

    S.fdef(U.method(:complement),
           args: S.cat(x: Proc),
           ret: Proc)
  end
end
