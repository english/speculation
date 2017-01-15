module Speculation
  module Specs
    using Speculation::NamespacedSymbols.refine(self)

    S = Speculation
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

    # TODO add args checking of blocks
    # S.fdef(U.method(:complement),
    #        args: S.cat(x: S.with_gen(Proc) do |r|
    #          val = S.gen(:any.ns(S)).call(r)
    #          -> (*args) { val }
    #        end),
    #        ret: Proc)

    S.fdef(U.method(:distinct?),
           args: S.cat(coll: S.coll_of(:any.ns(S))),
           ret: :boolean.ns(S))

    S.fdef(U.method(:ident?),
           args: S.cat(x: :any.ns(S)),
           ret: :boolean.ns(S))

    S.fdef(U.method(:method?),
           args: S.cat(x: :any.ns(S)),
           ret: :boolean.ns(S))
  end
end
