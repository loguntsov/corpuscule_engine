-record(corpuscule, {
  module :: atom(),
  id :: ce_corpuscule:id(),
  coords :: ce_coords:coords(),
  links =  [] :: [{ Id :: ce_corpuscule:id(), ce_link:link()}],
  options = #{} :: maps:map()
}).

-define(IS_CORPUSCULE(X), is_record(X, corpuscule)).
