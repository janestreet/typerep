open! Base

module type S0 = sig
  type t

  val name : string
end

[%%template
[@@@kind.default.explicit
  ka
  = ( any
    , any mod separable
    , value
    , value_or_null
    , float64
    , immediate64_or_null
    , immediate
    , immediate64
    , value mod external_
    , value mod external64
    , value_or_null mod external_
    , value_or_null mod external64 )]

module type S1 = sig
  type 'a t

  val name : string
end

[@@@kind.default.explicit kb = (ka, value)]

module type S2 = sig
  type ('a, 'b) t

  val name : string
end

[@@@kind.default.explicit kc = (ka, value)]

module type S3 = sig
  type ('a, 'b, 'c) t

  val name : string
end

[@@@kind.default.explicit kd = (ka, value)]

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val name : string
end

[@@@kind.default.explicit ke = (ka, value)]

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  val name : string
end]

[%%template
module type S1 = S1 [@kind.explicit value]
module type S2 = S2 [@kind.explicit value value]
module type S3 = S3 [@kind.explicit value value value]
module type S4 = S4 [@kind.explicit value value value value]
module type S5 = S5 [@kind.explicit value value value value value]]
