-module(spread).
%% reference on ex_canvas.erl in wx:demo()



-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([start/0, start/1, start_link/0, start_link/1, format/3,
  init/1, terminate/2, code_change/3,
  handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).


-record(state,
{
  win,
  parent,
  config,
  canvas,
  bitmap,
  overlay,
  pos
}).

%% For wx-2.9 usage
-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.

start() ->
  start([]).

start(Debug) ->
  wx_object:start(?MODULE, Debug, []).

start_link() ->
  start_link([]).

start_link(Debug) ->
  wx_object:start_link(?MODULE, Debug, []).

format(_Config, Str, Args) ->
  io:format(Str, Args),
  ok.

-define(DEBUG_NONE, 101).
-define(DEBUG_VERBOSE, 102).
-define(DEBUG_TRACE, 103).
-define(DEBUG_DRIVER, 104).

init(Options) ->
  wx:new(Options),
  process_flag(trap_exit, true),

  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wxErlang widgets", [{size, {1000, 500}}]),
  MB = wxMenuBar:new(),
  File = wxMenu:new([]),
  wxMenu:append(File, ?wxID_PRINT, "&Print code"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?wxID_EXIT, "&Quit"),
  Debug = wxMenu:new([]),
  wxMenu:appendRadioItem(Debug, ?DEBUG_NONE, "None"),
  wxMenu:appendRadioItem(Debug, ?DEBUG_VERBOSE, "Verbose"),
  wxMenu:appendRadioItem(Debug, ?DEBUG_TRACE, "Trace"),
  wxMenu:appendRadioItem(Debug, ?DEBUG_DRIVER, "Driver"),
  Help = wxMenu:new([]),
  wxMenu:append(Help, ?wxID_HELP, "Help"),
  wxMenu:append(Help, ?wxID_ABOUT, "About"),
  wxMenuBar:append(MB, File, "&File"),
  wxMenuBar:append(MB, Debug, "&Debug"),
  wxMenuBar:append(MB, Help, "&Help"),
  wxFrame:setMenuBar(Frame, MB),

  wxFrame:connect(Frame, command_menu_selected),
  wxFrame:connect(Frame, close_window),

  _SB = wxFrame:createStatusBar(Frame, []),

  %% Setup on toplevel because stc seems to steal this on linux
  wxFrame:dragAcceptFiles(Frame, true),
  wxFrame:connect(Frame, drop_files),

  %%   T        Uppersplitter
  %%   O        Left   |    Right
  %%   P  Widgets|Code |    Demo
  %%   S  -------------------------------
  %%   P          Log Window
  TopSplitter = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}]),
  %% Setup so that sizers and initial sizes, resizes the windows correct
  wxSplitterWindow:setSashGravity(TopSplitter, 0.5),

  %% LeftSplitter:



  %% Demo:

  %% UpperSplitter:

  %% TopSplitter:




  wxFrame:layout(Frame),
  wxFrame:show(Frame),

  %% Load the first example:

  %% The windows should be set up now, Reset Gravity so we get what we want
  wxSplitterWindow:setSashGravity(TopSplitter, 1.0),
  wxSplitterWindow:setMinimumPaneSize(TopSplitter, 1),

  wxToolTip:enable(true),
  wxToolTip:setDelay(500),

  {Frame, #state{
    win = Frame
  }}.



%%%%%%%%%%%%
%% Callbacks

%% Handled as in normal gen_server callbacks
handle_info({'EXIT', _, wx_deleted}, State) ->
  {noreply, State};
handle_info({'EXIT', _, shutdown}, State) ->
  {noreply, State};
handle_info({'EXIT', _, normal}, State) ->
  {noreply, State};
handle_info(Msg, State) ->
  format(State, "Got Info ~p~n", [Msg]),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  format(State, "Got Call ~p~n", [Msg]),
  {reply, ok, State}.

handle_cast(Msg, State) ->
  format(State, "Got cast ~p~n", [Msg]),
  {noreply, State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{id = Id,
  event = #wxCommand{type = command_menu_selected}},
    State = #state{}) ->
  case Id of
    ?wxID_PRINT ->
      %% If you are going to printout mainly text it is easier if
      %% you generate HTML code and use a wxHtmlEasyPrint
      %% instead of using DCs
      HEP = wxHtmlEasyPrinting:new([{name, "Print"},
        {parentWindow, State#state.win}]),
      Html = demo_html_tagger:erl2htmltext(?MODULE),
      wxHtmlEasyPrinting:previewText(HEP, Html),
      {noreply, State};
    ?DEBUG_TRACE ->
      wx:debug(trace),
      {noreply, State};
    ?DEBUG_DRIVER ->
      wx:debug(driver),
      {noreply, State};
    ?DEBUG_VERBOSE ->
      wx:debug(verbose),
      {noreply, State};
    ?DEBUG_NONE ->
      wx:debug(none),
      {noreply, State};
    ?wxID_HELP ->
      wx_misc:launchDefaultBrowser("http://www.erlang.org/doc/apps/wx/part_frame.html"),
      {noreply, State};
    ?wxID_ABOUT ->
      WxWVer = io_lib:format("~p.~p.~p.~p",
        [?wxMAJOR_VERSION, ?wxMINOR_VERSION,
          ?wxRELEASE_NUMBER, ?wxSUBRELEASE_NUMBER]),
      application:load(wx),
      {ok, WxVsn} = application:get_key(wx, vsn),
      AboutString =
        "Demo of various widgets\n"
        "Authors: Olle & Dan\n\n" ++
        "Frontend: wx-" ++ WxVsn ++
        "\nBackend: wxWidgets-" ++ lists:flatten(WxWVer),

      wxMessageDialog:showModal(wxMessageDialog:new(State#state.win, AboutString,
        [{style,
          ?wxOK bor
            ?wxICON_INFORMATION bor
            ?wxSTAY_ON_TOP},
          {caption, "About"}])),
      {noreply, State};
    ?wxID_EXIT ->
      {stop, normal, State};
    _ ->
      {noreply, State}
  end;
handle_event(#wx{event = #wxClose{}}, State = #state{win = Frame}) ->
  io:format("~p Closing window ~n", [self()]),
  ok = wxFrame:setStatusText(Frame, "Closing...", []),
  {stop, normal, State};
handle_event(Ev, State) ->
  format(State, "~p Got event ~p ~n", [?MODULE, Ev]),
  {noreply, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State = #state{win = Frame}) ->
  wxFrame:destroy(Frame),
  wx:destroy().

%%%%%%%%%%%%%%%%% Internals %%%%%%%%%%



-define(stc, wxStyledTextCtrl).




