module Speculation
  module Conj
    refine Hamster::Hash do
      def conj(v)
        merge(v)
      end
    end

    refine Hamster::Vector do
      def conj(x)
        add(x)
      end
    end

    refine Hamster::Set do
      def conj(x)
        add(x)
      end
    end

    refine Array do
      def conj(x)
        self + [x]
      end
    end

    refine Hash do
      def conj(x)
        k, v = x
        merge(k => v)
      end
    end

    refine Set do
      def conj(x)
        self + Set[x]
      end
    end
  end
end
