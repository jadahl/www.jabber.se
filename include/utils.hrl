%
%    Jabber.se Web Application
%    Copyright (C) 2010 Jonas Ådahl
%
%    This program is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Affero General Public License as
%    published by the Free Software Foundation, either version 3 of the
%    License, or (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Affero General Public License for more details.
%
%    You should have received a copy of the GNU Affero General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%

-ifndef(utils_hrl).
-define(utils_hrl, true).

-include("include/config.hrl").

%
% Utilities
%

%
% Configuration
%

-define(CONF(Key), cf_config:read(Key)).
-define(CONFM(Key), cf_config:read(?MODULE, Key)).


%
% ?T(Id) -> Text
%

-define(T(Id), i18n:t(Id)).

%
% CSS styles
%
% HIDDEN: display: none
% INLINE: display: inline
% BLOCK: display: block
%
-define(HIDDEN, "display: none").
-define(INLINE, "display: inline").
-define(BLOCK, "display: block").

%
% ?MAYBE_CONS(Expr, E, Es) -> [E | Es] | Es
%     Expr = E = Es = expr()
%
% If Expr evaluates to undefined, return Es, otherwise [E | Es]
%
-define(MAYBE_CONS(Expr, E, Es), case Expr of undefined -> Es; _ -> [E | Es] end).

%
% ?WHEN(expr(), statement()) ->
%     statement() | undefined
%
% If Condition evaluates to true, execute Statement, otherwise return undefined.
%
-define(WHEN(Condition, Statement), case Condition of true -> Statement; _ -> undefined end).

%
% ?WHEN_S(expr(), statement()) ->
%     statement() | ""
%
% If Condition evaluates to true, execute Statement, otherwise return "".
%
-define(WHEN_S(Condition, Statement), case Condition of true -> Statement; _ -> "" end).

% ?EITHER(expr(), statement(), statement()) ->
%     statement()
%
% If Condition evaluates to true, execute Statement1,
% otherwise execute Statement2
%
-define(EITHER(Condition, Statement1, Statement2), case Condition of true -> Statement1; _ -> Statement2 end).

%
% Exceptions
%

-define(THROW(Reason), throw({exception, Reason, ?MODULE, ?LINE})).

-define(CATCH_AND_WARN(Expr),
    try
        {ok, Expr}
    catch
        {exception, Reason, Module, Line} ->
            ?LOG_ERROR("Cought exception '~p', thrown at ~p:~p.~n", [Reason, Module, Line]),
            {error, Reason};
        E2 ->
            ?LOG_ERROR("Cought exception '~p'~n", [Exception]),
            {error, E2}
    end).

%
% AUTH(Statement) -> Statement
%
-define(AUTH(Statement), ?EITHER(session:authenticated(), (fun() -> session:env(), Statement end)(), session:unauthorized_request())).

%
% Logging
%

% Generic macro
-define(LOG_GENERIC(Level, Format, Arg), utils:log(Level, ?MODULE, ?LINE, Format, Arg)).

% LOG_INFO macro
-ifdef(ENABLE_LOG_INFO).
-define(LOG_INFO(Format, Arg), ?LOG_GENERIC(info, Format, Arg)).
-else.
-define(LOG_INFO(Format, Arg), ok).
-endif. % ENABLE_LOG_INFO

% LOG_WARNING macro
-ifdef(ENABLE_LOG_WARNING).
-define(LOG_WARNING(Format, Arg), ?LOG_GENERIC(warning, Format, Arg)).
-else.
-define(LOG_WARNING(Format, Arg), ok).
-endif. % ENABLE_LOG_WARNING

% LOG_ERROR macro
-ifdef(ENABLE_LOG_ERROR).
-define(LOG_ERROR(Format, Arg), ?LOG_GENERIC(error, Format, Arg)).
-else.
-define(LOG_ERROR(Format, Arg), ok).
-endif. % ENABLE_LOG_ERROR

-endif.
