-record(chargify_state, {
          subdomain :: string(),
          api_secret :: string()
          }).
-type chargify_state() :: #chargify_state{}.

-record(read_customer, {
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

          created_at :: string(),
          updated_at :: string()
          }).

-type read_customer() :: #read_customer{}.

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

-record(chargify_product, {
          id :: integer(),
          name :: string(),
          handle :: string(),
          trial_interval_unit :: string(),
          trial_interval :: string(),          
          trial_price_in_cents :: integer(),
          expiration_interval :: string(),          
          expiration_interval_unit :: string(),
          return_url :: string(),
          return_params :: string(),          
          description :: string(),
          accounting_code :: string(),
          initial_charge_in_cents :: integer(),
          interval :: string(),
          product_family :: tuple(),
          require_credit_card :: boolean(),          
          request_credit_card :: boolean(),
          interval_unit :: string(),
          price_in_cents :: integer(),
          created_at :: string(),          
          archived_at :: string(),
          updated_at :: string()
          }).
          
          
-type chargify_product() :: #chargify_product{}.
