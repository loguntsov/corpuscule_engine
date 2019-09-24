-module(ce_canvas).

-include_lib("wx/include/wx.hrl").

-export([
  start/0,
  draw/2
]).

-behaviour(wx_object).

-export([
  init/1,
  handle_info/2,
  handle_sync_event/3,
  handle_event/2,
  handle_call/3,
  code_change/3,
  terminate/2
]).

-record(state, {
  window,
  canvas,
  bitmap
}).

-include("corpuscule.hrl").

start() ->
  { ok, _ } = application:ensure_all_started(wx),
  wx_object:start_link(?MODULE, undefined, []).

draw(Window, World) ->
  wx_object:call(Window, { draw, World }).

%% Init is called in the new process.
init(undefined) ->
  wx:new(),
  Frame = wxFrame:new(wx:null(),
    -1, % window id
    "Corpuscul Engine", % window title
    [
      {size, {1024,800}}
    ]),

  %% if we don't handle this ourselves, wxwidgets will close the window
  %% when the user clicks the frame's close button, but the event loop still runs
  wxFrame:connect(Frame, close_window),

  Panel = wxPanel:new(Frame, [
  ]),

  %% Setup sizers
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Various shapes"}]),

  Button = wxButton:new(Panel, ?wxID_ANY, [
    {label, "Redraw"}
  ]),

  Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

  wxPanel:connect(Canvas, paint, [callback]),
  wxPanel:connect(Canvas, size),

  wxPanel:connect(Button, command_button_clicked),

  %% Add to sizers
  wxSizer:add(Sizer, Button, [
    {border, 5},
    {flag, ?wxALL}
  ]),
  wxSizer:addSpacer(Sizer, 5),
  wxSizer:add(Sizer, Canvas, [
    {flag, ?wxEXPAND},
    {proportion, 1}
  ]),

  wxSizer:add(MainSizer, Sizer, [
    {flag, ?wxEXPAND},
    {proportion, 1}
  ]),

  wxPanel:setSizer(Panel, MainSizer),
  wxSizer:layout(MainSizer),

  wxWindow:show(Frame),

  {W,H} = wxPanel:getSize(Canvas),
  Bitmap = wxBitmap:new(erlang:max(W,800),erlang:max(600,H)),
  {Frame, #state{
    window = Frame,
    canvas = Canvas,
    bitmap = Bitmap
  }}.

handle_sync_event(#wx{event = #wxPaint{}}, _wxObj,
  #state{canvas=Canvas, bitmap=Bitmap}) ->
  DC = wxPaintDC:new(Canvas),
  redraw(DC, Bitmap),
  wxPaintDC:destroy(DC),
  ok.


%% Handled as in normal gen_server callbacks
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call({draw, World}, _From, State) ->
  Fun = fun(DC) ->
    wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
    wxDC:setPen(DC, wxPen:new(?wxBLACK, [
      {width, 1}
    ])),
    Corpuscules = ce_world:all_corpuscules(World),
    {W,H} = wxPanel:getSize(State#state.canvas),
    CenterX = W div 2,
    CenterY = H div 2,
    lists:foreach(fun(Corpuscule) ->
      { X, Y, _Z } = coords3d(ce_corpuscule:coords(Corpuscule)),
      wxDC:setBrush(DC, ?wxBLUE_BRUSH),
      BX = round(CenterX+X),
      BY = round(CenterY+Y),
      wxDC:drawCircle(DC, { BX, BY }, 2),
      lists:foreach(fun({Id, _ }) ->
        { XL, YL, _ZL } = coords3d(ce_corpuscule:coords(ce_world:get_corpuscule(World, Id))),
        CX = round(CenterX+XL),
        CY = round(CenterY+YL),
        wxDC:drawLine(DC, { BX, BY }, {CX, CY })
      end, ce_corpuscule:links(Corpuscule))
    end, Corpuscules)
  end,
  draw(State#state.canvas, State#state.bitmap, Fun),
  { reply, ok, State };


handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event=#wxClose{}}, State = #state{window =Frame}) ->
  io:format("~p Closing window ~n",[self()]),
  wxWindow:destroy(Frame),
  {stop, normal, State};

handle_event(#wx{} = Msg, State = #state{window = Frame}) ->
  logger:info("Message ~p", [ Msg ]),
  { noreply, State }.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  erlang:halt(),
  ok.

%% INTERNAL

draw(Canvas, Bitmap, Fun) ->
  MemoryDC = wxMemoryDC:new(Bitmap),
  wxDC:clear(MemoryDC),
  Fun(MemoryDC),

  CDC = wxWindowDC:new(Canvas),
  wxDC:blit(CDC, {0,0},
    {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
    MemoryDC, {0,0}),
  wxWindowDC:destroy(CDC),
  wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
  MemoryDC = wxMemoryDC:new(Bitmap),
  wxDC:blit(DC, {0,0},
    {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
    MemoryDC, {0,0}),
  wxMemoryDC:destroy(MemoryDC).

coords3d([X, Y]) -> { X, Y, 0};
coords3d([X, Y, Z | _ ]) -> { X, Y, Z }.

