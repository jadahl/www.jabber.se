-ifndef(utils_hrl).
-define(utils_hrl, true).

-include("include/config.hrl").

%
% Utilities
%

%
% ?UI(UI) -> Elements
%   UI = ui(),
%   Elements = [element()]
%
-define(UI(UI), stk_ui:render(UI)).

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
