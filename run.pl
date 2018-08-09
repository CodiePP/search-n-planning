:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(planner).

:- http_handler('/', random_story, []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).


random_story(Request) :-
  reply_html_page(
     title('Story'),
     [\page_content(Request)]).

page_content(Request) -->
  html(
     [
      h1('Random story'),
      div(\nav_bar),
      %p('The body goes here'),
      div(\story(Request)),
      div(\html_receive(bottom_nav))
     ]).


story(_Request) -->
  { with_output_to(string(Story),
       planning(
          [person_in_room('Little Paul', bathroom),
           tool_in_room(hammer,garage), tool_in_room(vacuum,kitchen),
           tool_in_room(scrub,corridor)
          ],  % initial state
          [person_in_room('Little Paul', bathroom), tool_in_hands('Little Paul', vacuum)],     % goal to be reached
          [],                              % cannot pass through this state
          _))
  },
  html( [pre(Story)] ).

nav_bar -->
  {
      findall(Name, nav(Name, _), ButtonNames),
      maplist(as_top_nav, ButtonNames, TopButtons),
      maplist(as_bottom_nav, ButtonNames, BottomButtons)
  },
  html([\html_post(bottom_nav, BottomButtons) | TopButtons]).


nav('generate another story ', '/').

as_top_nav(Name, a([href=HREF, class=topnav], Name)) :-
  nav(Name, HREF).

as_bottom_nav(Name, a([href=HREF, class=bottomnav], Name)) :-
  nav(Name, HREF).

% start
:- server(8080).
