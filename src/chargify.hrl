-record(chargify_state, {
          subdomain :: string(),
          api_secret :: string()
          }).
-type chargify_state() :: #chargify_state{}.

-record(customer, {
          first_name :: string(),
          last_name :: string(),
          email :: string(),
          organization :: string(),
          reference :: string()
          }).


-type customer() :: #customer{}.
