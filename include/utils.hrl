-ifndef(utils_hrl).
-define(utils_hrl, true).

-include("include/config.hrl").

%
% Utilities
%

%
% ?WHEN(expr(), statement()) ->
%     statement() | undefined
%
% If Condition evaluates to true, execute Statement, otherwise return undefined.
%
-define(WHEN(Condition, Statement), case Condition of true -> Statement; _ -> undefined end).

% ?EITHER(expr(), statement(), statement()) ->
%     statement()
%
% If Condition evaluates to true, execute Statement1,
% otherwise execute Statement2
%
-define(EITHER(Condition, Statement1, Statement2), case Condition of true -> Statement1; _ -> Statement2 end).

%
% Logging
%

% LOG_INFO macro
-ifdef(ENABLE_LOG_INFO).
-define(LOG_INFO(Format, Arg), utils:log(info, ?MODULE, Format, Arg)).
-else.
-define(LOG_INFO(Format, Arg), ok).
-endif. % ENABLE_LOG_INFO

% LOG_WARNING macro
-ifdef(ENABLE_LOG_WARNING).
-define(LOG_WARNING(Format, Arg), utils:log(warning, ?MODULE, Format, Arg)).
-else.
-define(LOG_WARNING(Format, Arg), ok).
-endif. % ENABLE_LOG_WARNING

% LOG_ERROR macro
-ifdef(ENABLE_LOG_ERROR).
-define(LOG_ERROR(Format, Arg), utils:log(error, ?MODULE, Format, Arg)).
-else.
-define(LOG_ERROR(Format, Arg), ok).
-endif. % ENABLE_LOG_ERROR

-endif.
