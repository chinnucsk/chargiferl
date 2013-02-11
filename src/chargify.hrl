-record(chargify_state, {
          subdomain :: string(),
          api_secret :: string()
          }).
-type chargify_state() :: #chargify_state{}.

-record(customer, {
          id      :: integer(),
          email :: string(),
          
          first_name :: string(),
          last_name :: string(),
          
          address :: string(),
          address_2 :: string(),
          city    :: string(),
          state   :: string(),
          zip :: string(),          
          country :: string(),
          phone   :: string(),
          
          organization :: string(),
          reference :: string(),
          vat_number :: string(),
          
          updated_at :: string(),
          created_at :: string()
          }).

-type customer() :: #customer{}.

-record(create_customer, {
          email :: string(),
          
          first_name :: string(),
          last_name :: string(),
          
          address :: string(),
          address_2 :: string(),
          city    :: string(),
          state   :: string(),
          zip :: string(),          
          country :: string(),
          phone   :: string(),
          
          organization :: string(),
          reference :: string()
          }).

-type create_customer() :: #create_customer{}.

-record(address, {
          address :: string(),
          address_2 :: string(),
          city    :: string(),
          state   :: string(),
          zip :: string(),          
          country :: string(),
          phone   :: string()
          }).

-type address() :: #address{}.
          
