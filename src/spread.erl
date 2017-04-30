-module(spread).
%% reference on ex_canvas.erl in wx:demo()

-behavior(wx_object).

%% client API
-export([start/1, start/0]).

%% wx_object callbacks
-export([init/1, handle_event/2, handle_sync_event/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-record(state,
{
  parent
  , config
  , canvas
  , bitmap
  , overlay
  , pos
}).

start() -> start([]).
start(Config) -> wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implement behaviour of wx_object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
  wx:new(Config),
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
%%  Parent = proplists:get_value(parent, Config),

  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Spread", [{size, {1000, 500}}]),
  MB = wxMenuBar:new(),
  File = wxMenu:new([]),
  wxMenu:append(File, ?wxID_EXIT, "&Quit"),
  wxMenuBar:append(MB, File, "&File"),
  wxFrame:setMenuBar(Frame, MB),

  wxFrame:connect(Frame, command_menu_selected),
  wxFrame:connect(Frame, close_window),

  Parent = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}]),
  wxSplitterWindow:setSashGravity(Parent, 0.5),
  Panel = wxPanel:new(Parent, []),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Spread"}]),
  Button = wxButton:new(Panel, ?wxID_ANY, [{label, "Restart"}]),
  Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

  wxPanel:connect(Canvas, paint, [callback]),
  wxPanel:connect(Canvas, size),
  wxPanel:connect(Canvas, left_down),
  wxPanel:connect(Canvas, left_up),
  wxPanel:connect(Canvas, motion),

  wxPanel:connect(Button, command_button_clicked),

  %% link up the GUI components
  wxSizer:add(Sizer, Button, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:addSpacer(Sizer, 5),
  wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND}, {proportion, 1}]),

  wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),

  wxPanel:setSizer(Panel, MainSizer),
  wxSizer:layout(MainSizer),

  {W, H} = wxPanel:getSize(Canvas),
  Bitmap = wxBitmap:new(max(W, 30), max(30, H)),

  wxFrame:show(Frame),

  {Panel, #state{
    parent = Parent
    , config = Config
    , canvas = Canvas
    , bitmap = Bitmap
    , overlay = wxOverlay:new()
  }}.

handle_sync_event(#wx{event = #wxPaint{}}, _wxObj,
    #state{canvas = Canvas, bitmap = Bitmap}) ->
  DC = wxPaintDC:new(Canvas),
  redraw(DC, Bitmap),
  wxPaintDC:destroy(DC),
  ok.

handle_event(#wx{event = #wxCommand{type = command_button_clicked}}, State = #state{}) ->
  {W, H} = wxPanel:getSize(State#state.canvas),
  Positions = lists:map(fun(_) -> get_pos(W, H) end, lists:seq(1, (W + H) div 20)),
  Fun = fun(DC) ->
    wxDC:clear(DC),
    lists:foreach(fun({X, Y} = Pos) ->
      wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
      wxDC:setPen(DC, wxPen:new(?wxBLACK, [{width, 2}])),
      %% TODO
      wxDC:drawLabel(DC, "Text", {X, Y, 60, 20})
                  end, Positions)
        end,
  draw(State#state.canvas, State#state.bitmap, Fun),
  {noreply, State};
handle_event(#wx{event = #wxSize{size = {W, H}}}, State = #state{bitmap = Prev, canvas = Canvas}) ->
  if
    W > 0 andalso H > 0 ->
      Bitmap = wxBitmap:new(W, H),
      draw(Canvas, Bitmap, fun(DC) -> wxDC:clear(DC) end),
      wxBitmap:destroy(Prev),
      {noreply, State#state{bitmap = Bitmap}};
    true ->
      {noreply, State}
  end;
handle_event(#wx{event = #wxMouse{type = left_down, x = X, y = Y}}, State) ->
  {noreply, State#state{pos = {X, Y}}};
handle_event(#wx{event = #wxMouse{type = motion, x = X1, y = Y1}},
    #state{pos = Start, overlay = Overlay, canvas = Canvas} = State) ->
  case Start of
    undefined -> ignore;
    {X0, Y0} ->
      DC = wxClientDC:new(Canvas),
      DC0 = wxDCOverlay:new(Overlay, DC),
      wxDCOverlay:clear(DC0),
      wxDC:setPen(DC, ?wxLIGHT_GREY_PEN),
      wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
      wxDC:drawRectangle(DC, {X0, Y0, X1 - X0, Y1 - Y0}),
      wxDCOverlay:destroy(DC0),
      wxClientDC:destroy(DC)
  end,
  {noreply, State};
handle_event(#wx{event = #wxMouse{type = left_up}},
    #state{overlay = Overlay, canvas = Canvas} = State) ->
  DC = wxClientDC:new(Canvas),
  DC0 = wxDCOverlay:new(Overlay, DC),
  wxDCOverlay:clear(DC0),
  wxDCOverlay:destroy(DC0),
  wxClientDC:destroy(DC),
  wxOverlay:reset(Overlay),
  {norepl, State#state{pos = undefined}};
handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got event ~p~n", [Ev]),
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks for gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(shutdown, _From, State = #state{parent = Panel}) ->
  wxPanel:destroy(Panel),
  {stop, normal, ok, State};
handle_call(Request, _From, State) ->
  io:format("Got call ~p~n", [Request]),
  {reply, {error, nyi}, State}.

handle_cast(Request, State) ->
  io:format("Got cast ~p~n", [Request]),
  {noreply, State}.

handle_info(Info, State) ->
  io:format("Got Info ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, State) ->
  wxOverlay:destroy(State#state.overlay),
  ok.

code_change(_OldVsn, _State, Extra) ->
  {stop, ignore, Extra}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw(Canvas, Bitmap, Fun) ->
  MemoryDC = wxMemoryDC:new(Bitmap),
  Fun(MemoryDC),

  CDC = wxWindowDC:new(Canvas),
  wxDC:blit(CDC, {0, 0},
    {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
    MemoryDC, {0, 0}),
  wxWindowDC:destroy(CDC),
  wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
  MemoryDC = wxMemoryDC:new(Bitmap),
  wxDC:blit(DC, {0, 0},
    {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
    MemoryDC, {0, 0}),
  wxMemoryDC:destroy(MemoryDC).

get_pos(W, H) ->
  {rand:uniform(W), rand:uniform(H)}.
