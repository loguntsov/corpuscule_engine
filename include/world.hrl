-record(world, {
  space :: ce_space:space(),
  time = 0 :: pos_integer(),
  time_multiplicator = 1 :: number(),
  corpuscules = #{}:: maps:map(ce_corpuscule:id(), ce_corpuscule:corpuscule()),
  distances = gb_trees:empty() :: gb_trees:tree(),
  interactions :: [atom()]
}).

-define(IS_WORLD(X), is_record(X, world)).
