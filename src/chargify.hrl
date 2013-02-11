-record(state, {
          subdomain :: string(),
          api_secret :: string()
          }).
-type state() :: #state{}.

-record(customer, {
          first_name :: string(),
          last_name :: string(),
          email :: string(),
          organization :: string(),
          reference :: string()
          }).


-type customer() :: #customer{}.
