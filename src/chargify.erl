%%%-------------------------------------------------------------------
%%% @author Christopher Brown <cb@opscode.com>
%%% @copyright (C) 2013, Christopher Brown
%%% @doc
%%% gen_server API to Chargify
%%% @end
%%% Created : 10 Feb 2013 by Christopher Brown <cb@opscode.com>
%%% @author Christopher Brown <cb@opscode.com>
%%%-------------------------------------------------------------------

-module(chargify).
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API interface
-export([
         list_customers/0,
         customer_by_id/1,
         customer_by_reference/1,
         create_customer/1
         % update_customer/1,
         % customer_subscriptions/1,
         % subscription/1,
         % create_subscription/1,
         % update_subscription/2,
         % cancel_subscription/2,
         % reactivate_subscription/1,
         % charge_subscription/2,
         % migrate_subscription/2,
         % adjust_subscription/2,
         % list_products/0,
         % product/1,
         % product_by_handle/1,
         % list_subscription_usage/2,
         % subscription_transactions/2,
         % subscription_statements/2,
         % site_transactions/1,
         % list_components/1,
         % subscription_component/2,
         % update_subscription_component_allocated_quantity/3,
         % update_subscription_component_enabled/3,
         % update_subscription_component/3
         ]).

-ifdef(debug).
-compile(export_all).
-endif.

-define(SERVER, ?MODULE). 

-record(state, {
          subdomain :: string(),
          api_secret :: string()
          }).
-type state() :: #state{}.

-type value() :: term().
-type header() :: atom() | string().
-type headerlist() :: [{header(), value()}].
-type method() :: get | post | head | options | put | delete | trace | mkcol | propfind | proppatch | lock | unlock | move | copy.
%% -type status() :: string().
%% -type responseheaders() :: [respHeader()].
%% -type respheader() :: {headerName(), headerValue()}.
%% -type headername() :: string().
%% -type headervalue() :: string().
-type response() :: {ok, status, responseheaders, responsebody} | {ibrowse_req_id, req_id()} | {error, reason}.
-type req_id() :: term().
%% -type responsebody() :: string() | {file, filename}.
%% -type reason() :: term().
%% -type filename() :: string().
-type url() :: string().
-type body() :: string().


-record(customer, {
          first_name :: string(),
          last_name :: string(),
          email :: string(),
          organization :: string(),
          reference :: string()
          }).


-type customer() :: #customer{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], [{debug, []}]).

stop() ->
    case catch gen_server:call(ibrowse, stop) of
        {'EXIT',{noproc,_}} ->
            ok;
        Res ->
            Res
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec send_req(url(), headerlist(), method()) -> response().
send_req(Url, HeaderList, Method) ->
    ibrowse:send_req(Url, HeaderList, Method).

-spec send_req(url(), headerlist(), method(), body()) -> response().
send_req(Url, HeaderList, Method, Body) ->
    ibrowse:send_req(Url, HeaderList, Method, Body).

-spec get(url(), headerlist()) -> response().
get(Url, HeaderList) ->
    send_req(Url, HeaderList, get).

% -spec put(url(), headerlist(), body()) -> response().
% put(Url, HeaderList, Body) ->
%     send_req(Url, HeaderList, put, Body).

-spec post(url(), headerlist(), body()) -> response().
post(Url, HeaderList, Body) ->
    send_req(Url, HeaderList, post, Body).

-spec build_url(state(), string()) -> string().
build_url(ChargifyState, ResourcePath) ->
    "https://" ++ ChargifyState#state.subdomain ++ ".chargify.com" ++ ResourcePath.

-spec build_body(tuple()) -> string().
build_body(Body) ->
    ejson:encode({[Body]}).

-spec add_auth(state(), headerlist()) -> headerlist().
add_auth(ChargifyState, HeaderList) ->
    AuthorizationData = "Basic " ++ binary_to_list(base64:encode(ChargifyState#state.subdomain ++ ":x")),
    [{"Authorization", AuthorizationData } | HeaderList].

-spec list_customers() -> [customer()].
list_customers() ->
    ChargifyState = #state{subdomain = "opscode-preprod",
                                    api_secret = "mTTHZMYQZyR72g-bGkux"},
    ResourcePath = "/customers.json",
    get(build_url(ChargifyState, ResourcePath), add_auth(ChargifyState,[{accept, "application/json"}])).

-spec customer_by_id(string() | integer()) -> customer().
customer_by_id(ChargifyId) when is_integer(ChargifyId) ->
    customer_by_id(binary_to_list(ChargifyId));
customer_by_id(ChargifyId) when is_list(ChargifyId) ->
    ChargifyState = #state{subdomain = "opscode-preprod",
                                    api_secret = "mTTHZMYQZyR72g-bGkux"},
    ResourcePath = "/customers/" ++ ChargifyId ++ ".json",
    get(build_url(ChargifyState, ResourcePath), add_auth(ChargifyState,[{accept, "application/json"}])).

-spec customer_by_reference(string()) -> customer().
customer_by_reference(ReferenceId) ->
    ChargifyState = #state{subdomain = "opscode-preprod",
                                    api_secret = "mTTHZMYQZyR72g-bGkux"},
    ResourcePath = "/customers/lookup.json?reference=" ++ ReferenceId,
    get(build_url(ChargifyState, ResourcePath), add_auth(ChargifyState,[{accept, "application/json"}])).

-spec create_customer(customer()) -> string().
create_customer(Info) ->
    ChargifyState = #state{subdomain = "opscode-preprod",
                                    api_secret = "mTTHZMYQZyR72g-bGkux"},    
    ResourcePath = "/customers.json",
    post(build_url(ChargifyState, ResourcePath), add_auth(ChargifyState,[{accept, "application/json"}]), build_body({customer, Info})).

%     % * first_name (Required)
%     % * last_name (Required)
%     % * email (Required)
%     % * organization (Optional) Company/Organization name
%     % * reference (Optional, but encouraged) The unique identifier used within your own application for this customer
%     % 
% update_customer(_Info) ->
%     %   info.stringify_keys!
%     %   chargify_id = info.delete('id')
%     %   response = Hashie::Mash.new(put("/customers/#{chargify_id}.json", :body => {:customer => info}))
%     %   return response.customer unless response.customer.to_a.empty?
%     %   response
%     % end
%     pass().

% customer_subscriptions(_ChargifyId) ->
%     %   subscriptions = get("/customers/#{chargify_id}/subscriptions.json")
%     %   subscriptions.map{|s| Hashie::Mash.new s['subscription']}
%     % end
%     pass().    

% subscription(_SubscriptionId) ->
%     %   raw_response = get("/subscriptions/#{subscription_id}.json")
%     %   return nil if raw_response.code != 200
%     %   Hashie::Mash.new(raw_response).subscription
%     % end
%     pass().

%     % Returns all elements outputted by Chargify plus:
%     % response.success? -> true if response code is 201, false otherwise
% create_subscription(_SubscriptionAttributes) ->
%     %   raw_response = post("/subscriptions.json", :body => {:subscription => subscription_attributes})
%     %   created  = true if raw_response.code == 201
%     %   response = Hashie::Mash.new(raw_response)
%     %   (response.subscription || response).update(:success? => created)
%     % end
%     pass().

%     % Returns all elements outputted by Chargify plus:
%     % response.success? -> true if response code is 200, false otherwise
% update_subscription(_SubId, _SubscriptionAttributes) ->
%     %   raw_response = put("/subscriptions/#{sub_id}.json", :body => {:subscription => subscription_attributes})
%     %   updated      = true if raw_response.code == 200
%     %   response     = Hashie::Mash.new(raw_response)
%     %   (response.subscription || response).update(:success? => updated)
%     % end
%     pass().

%     % Returns all elements outputted by Chargify plus:
%     % response.success? -> true if response code is 200, false otherwise
% cancel_subscription(_SubId, _Message) ->
%     %   raw_response = delete("/subscriptions/#{sub_id}.json", :body => {:subscription => {:cancellation_message => message} })
%     %   deleted      = true if raw_response.code == 200
%     %   response     = Hashie::Mash.new(raw_response)
%     %   (response.subscription || response).update(:success? => deleted)
%     % end
%     pass().

% reactivate_subscription(_SubId) ->
%     %   raw_response = put("/subscriptions/#{sub_id}/reactivate.json", :body => "")
%     %   reactivated  = true if raw_response.code == 200
%     %   response     = Hashie::Mash.new(raw_response) rescue Hashie::Mash.new
%     %   (response.subscription || response).update(:success? => reactivated)
%     % end
%     pass().

% charge_subscription(_SubId, _SubscriptionAttributes) ->
%     %   raw_response = post("/subscriptions/#{sub_id}/charges.json", :body => { :charge => subscription_attributes })
%     %   success      = raw_response.code == 201
%     %   if raw_response.code == 404
%     %     raw_response = {}
%     %   end

%     %   response = Hashie::Mash.new(raw_response)
%     %   (response.charge || response).update(:success? => success)
%     % end
%     pass().

% migrate_subscription(_SubId, _ProductId) ->
%     %   raw_response = post("/subscriptions/#{sub_id}/migrations.json", :body => {:product_id => product_id })
%     %   success      = true if raw_response.code == 200
%     %   response     = Hashie::Mash.new(raw_response)
%     %   (response.subscription || {}).update(:success? => success)
%     % end
%     pass().

% adjust_subscription(_SubId, _Attributes) ->
%     %   raw_response = post("/subscriptions/#{sub_id}/adjustments.json",
%     %                       :body => { :adjustment => attributes })
%     %   created = true if raw_response.code == 201
%     %   response = Hashie::Mash.new(raw_response)
%     %   (response.adjustment || response).update(:success? => created)
%     % end
%     pass().

% list_products() ->
%       % products = get("/products.json")
%       % products.map{|p| Hashie::Mash.new p['product']}
%     pass().
    
% product(_ProductId) ->
% %      Hashie::Mash.new( get("/products/#{product_id}.json")).product
%    pass().
    
% product_by_handle(_Handle) ->
% %      Hashie::Mash.new(get("/products/handle/#{handle}.json")).product
%    pass().
    
% list_subscription_usage(_SubscriptionId, _ComponentId) ->
%       % raw_response = get("/subscriptions/#{subscription_id}/components/#{component_id}/usages.json")
%       % success      = raw_response.code == 200
%       % response     = Hashie::Mash.new(raw_response)
%       % response.update(:success? => success)
%    pass().
    
% subscription_transactions(_SubId, _Options) ->
%       % transactions = get("/subscriptions/#{sub_id}/transactions.json", :query => options)
%       % transactions.map{|t| Hashie::Mash.new t['transaction']}
%    pass().

% subscription_statements(_SubId, _Options) ->
%       % statements = get("/subscriptions/#{sub_id}/statements.json", :query => options)
%       % statements.map{|t| Hashie::Mash.new t['statement']}
%    pass().

% site_transactions(_Options) ->
%       % transactions = get("/transactions.json", :query => options)
%       % transactions.map{|t| Hashie::Mash.new t['transaction']}
%    pass().

% list_components(_SubscriptionId) ->
%       % components = get("/subscriptions/#{subscription_id}/components.json")
%       % components.map{|c| Hashie::Mash.new c['component']}
%    pass().
    
% subscription_component(_SubscriptionId, _ComponentId) ->
%       % response = get("/subscriptions/#{subscription_id}/components/#{component_id}.json")
%       % Hashie::Mash.new(response).component
%    pass().
    
% update_subscription_component_allocated_quantity(_SubscriptionId, _ComponentId, _Quantity) ->
% %      update_subscription_component(subscription_id, component_id, :allocated_quantity => quantity)
%    pass().

% update_subscription_component_enabled(_SubscriptionId, _ComponentId, _Enabled) ->
% %      update_subscription_component(subscription_id, component_id, :enabled => enabled)
%    pass().

% update_subscription_component(_SubscriptionId, _ComponentId, _Component) ->
%       % component[:enabled] = (component[:enabled] ? 1 : 0) if component.keys.include?(:enabled)
%       % response = put("/subscriptions/#{subscription_id}/components/#{component_id}.json", 
%       %               :body => {:component => component})
%       % response[:success?] = response.code == 200
%       % Hashie::Mash.new(response)
%    pass(). 

    
