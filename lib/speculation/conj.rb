module Speculation
  module Conj
    refine Hamster::Hash do
      def conj(x)
        if Utils.array?(x)
          unless x.count == 2
            raise ArgumentError, "Array arg to conj must be a pair"
          end

          merge(x[0] => x[1])
        else
          merge(x)
        end
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
        if Utils.array?(x)
          unless x.count == 2
            raise ArgumentError, "Array arg to conj must be a pair"
          end

          merge(x[0] => x[1])
        else
          merge(x)
        end
      end
    end

    refine Set do
      def conj(x)
        self + Set[x]
      end
    end
  end
end
