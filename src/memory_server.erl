-module(memory_server).
-include("bifrost.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-behavior(gen_bifrost_server).

-export([login/3, 
         init/2, 
         make_directory/2,
		 current_directory/1,
         change_directory/2, 
         list_files/2, 
         remove_directory/2, 
         remove_file/2, 
         put_file/4, 
         get_file/2,
         file_info/2,
         rename_file/3,
         site_command/3,
         site_help/1,
         disconnect/1]).
-ifdef(debug).
-compile(export_all).
-endif.

%% -ifdef(TEST).
%% -export([fs_with_paths/1, fs_with_paths/2, add_file/3]).
%% -endif.

-define(OPEN_FILE_OPTIONS,[read,raw,binary]).
-define(WRITE_FILE_OPTIONS,[write,raw,binary]).


init(InitialState, _) ->
    InitialState.

login(State=#connection_state{users=Users}, Username, Password) ->
%% 	Temp = {Username,Password},
%% 	Login = lists:member(Temp, Users),
%% 	?INFO_F("~p --- isLogin:~p ~n",[?MODULE,Login]),
    {true, State}.

current_directory(State) ->
	State#connection_state.current_dir.

make_directory(State, Directory) ->
	ClientDir = filename:join(State#connection_state.current_dir,Directory),
    Target = State#connection_state.root_dir ++ ClientDir,
	
	case file:make_dir(Target) of
		ok ->
			{ok,State#connection_state{current_dir=ClientDir}};
		{error,_Reson} ->
			?DEBUG_F("~p -- ~p Target error:~p ~n ",[?MODULE,Target,_Reson]),
			{error,eexist}
	end.

change_directory(State, Directory) ->
	ClientDir = filename:join(State#connection_state.current_dir,Directory),
	Target = State#connection_state.root_dir ++ ClientDir,
	
	case filelib:is_file(Target) of
		true ->
            {ok, State#connection_state{current_dir=ClientDir}};
		false ->
		    io:format("error while listing directory ~p~n", [Target]),
		    {error,State}
	end.

disconnect(_) ->
    ok.

remove_file(State, File) ->
	Target = [State#connection_state.root_dir,State#connection_state.current_dir,"/",File],
	Filename = filename:join(string:tokens(Target, "/")),
	
	?INFO_F("~p --- Filename:~p ~n",[?MODULE,Filename]),
	case file:delete(Filename) of
		ok ->
			{ok,State};
		{error, Reason} ->
			?DEBUG_F("~p --- remove_file,error:~p ~n",[?MODULE,Reason]),
			{error, Reason}
	end.

rename_file(State, FromPath, ToPath) ->
	TargetFromPath = [State#connection_state.root_dir,State#connection_state.current_dir,"/",FromPath],
	FileFromPath = filename:join([TargetFromPath]),
	
	TargetToPath = [State#connection_state.root_dir,State#connection_state.current_dir,"/",ToPath],
	FileToPath = filename:join([TargetToPath]),
	
	case file:rename(FileFromPath, FileToPath) of
		ok ->
			{ok,State};
		{error,Reason} ->
			?ERROR_F("~p --- rename_file,error:~p ~n",[?MODULE,Reason]),
			{error, rename_error}
	end.		

remove_directory(State, Directory) ->
	Target = [State#connection_state.root_dir,State#connection_state.current_dir,Directory],
	FilePath = filename:join([Target]),
	?INFO_F("~p --- FilePath:~p ~n",[?MODULE,FilePath]),
	
	case file:del_dir(FilePath) of
		ok ->
			{ok,State};
		{error, Reason} ->
			?DEBUG_F("~p --- remove_directory,error:~p ~n",[?MODULE,Reason]),
			{error,Reason}
	end.


list_files(State, "-al") ->
	{error, State};
list_files(State, _Directory) ->
	Target = [State#connection_state.root_dir,State#connection_state.current_dir],
	FileDir = filename:join([Target]),
	
	case file:list_dir(FileDir) of
		{ok,FileList} ->
			FileInfoList = lists:foldl(fun(FileName, AccIn) -> 
						FilePath = filename:join(FileDir,FileName),
						{ok,FileInfo} = file:read_file_info(FilePath),
						[{FileInfo,FileName}|AccIn]
						end, [], FileList),
			FileInfoList;
		{error, Reason} ->
			?INFO_F("~p list_files,error:~p ~n",[?MODULE,Reason]),
			{error, State}
	end.

% mode could be append or write, but we're only supporting
% write.
% FileRetrievalFun is fun(ByteCount) and retrieves ByteCount bytes
%  and returns {ok, Bytes, Count} or done
put_file(State, ProvidedFileName, _Mode, FileRetrievalFun) ->
	Target = [State#connection_state.root_dir,State#connection_state.current_dir,"/",ProvidedFileName],
	FilePath = filename:join([Target]),
	
	{ok,DataFD} = prim_file:open(FilePath,?WRITE_FILE_OPTIONS),
	{ok, _Count} = write_from_fun(DataFD,FileRetrievalFun),
	prim_file:close(DataFD),
	
    {ok, State}.

    
get_file(State, Path) ->
	Target = State#connection_state.root_dir ++ State#connection_state.current_dir ++ "/" ++ Path,
	FilePath = filename:join([Target]),
	
	{ok,DataFD} = prim_file:open(FilePath,?OPEN_FILE_OPTIONS),
    {ok, reading_fun(State, DataFD)}.


file_info(State, Path) ->
	?INFO_F("~p ~n",["file_info"]),
    {error, not_found}.

site_command(_, _, _) ->
    {error, not_found}.

site_help(_) ->
    {error, not_found}.


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
write_from_fun(DataFD,Fun) ->
    write_from_fun(DataFD, 0, Fun).
write_from_fun(DataFD, Count, Fun) ->
    case Fun(1024) of
        {ok, Bytes, ReadCount} ->
			prim_file:write(DataFD, Bytes),
            write_from_fun(DataFD, Count + ReadCount, Fun);
        done ->
            io:format("DONE!"),
            {ok, Count}
    end.

reading_fun(State, DataFD) ->
	fun(ByteCount) ->
			case prim_file:read(DataFD, ByteCount) of
				{ok,Data} ->
					{ok,Data,reading_fun(State, DataFD)};
				eof ->
					prim_file:close(DataFD),
					{done,State}
			end
	end.


%% Tests
%% -ifdef(TEST).
%% 
%% fs_with_paths([], State) ->
%%     State;
%% fs_with_paths([Path | Paths], State) ->
%%     {ok, NewState} = make_directory(State, Path),
%%     fs_with_paths(Paths, NewState).
%% 
%% fs_with_paths(Paths) ->
%%     fs_with_paths(Paths, wrap_fs(create_fs())).
%% 
%% add_file(State, Path, Contents) ->
%%     put_file(State, 
%%              Path, 
%%              image, 
%%              fun(_) ->
%%                      {ok, Contents, size(Contents)}
%%              end).
%% 
%% wrap_fs(Fs) ->
%%     #connection_state{module_state=#msrv_state{fs=Fs}, module=?MODULE}.
%% 
%% create_fs() ->
%%     new_directory("").
%% 
%% new_file_info_test() ->
%%     FileInfo = #file_info{name="Test",
%%                           mtime=erlang:localtime(),
%%                           type=file,
%%                           mode=0511,
%%                           gid=0,
%%                           uid=0,
%%                           size=20},
%%     FileInfo = new_file_info("Test", file, 20).
%% 
%% new_directory_test() ->
%%     EmptyDictionary = dict:new(),
%%     NewFileInfo = new_file_info("Test", dir, 0),
%%     {dir, EmptyDictionary, NewFileInfo} = new_directory("Test").
%% 
%% login_test() ->
%%     OldState = #connection_state{},
%%     NewState = #connection_state{module_state=#msrv_state{current_dir=[[]]}},
%%     {true, NewState} = login(OldState, "a", "b").
%% 
%% current_directory_test() ->
%%     ModState1 = #msrv_state{current_dir = [[]]},
%%     ModState2 = #msrv_state{current_dir = [[], "testing", "123"]},
%%     State1 = #connection_state{module_state=ModState1},
%%     State2 = #connection_state{module_state=ModState2},
%%     "/" = current_directory(State1),
%%     "/testing/123" = current_directory(State2).
%% 
%% change_directory_test() ->
%%     FS = set_path(create_fs(), ["testing", "123"], new_directory("123")),
%%     ModStateBefore = #msrv_state{current_dir = [[], "testing", "123"], fs=FS},
%%     ModStateAfter = #msrv_state{current_dir = [[], "testing"], fs=FS},
%%     StateBefore = #connection_state{module_state=ModStateBefore},
%%     StateAfter = #connection_state{module_state=ModStateAfter},
%%     {ok, StateBefore} = change_directory(StateBefore, "."),
%%     {ok, StateAfter} = change_directory(StateBefore, ".."),
%%     {ok, StateAfter} = change_directory(StateBefore, "/testing"),
%%     {error, StateBefore} = change_directory(StateBefore, "/magical/unicorn").
%% 
%% remove_directory_test() ->
%%     FSBefore = set_path(create_fs(), ["testing", "123"], new_directory("123")),
%%     FSAfter = set_path(create_fs(), ["testing"], new_directory("testing")),
%%     FSWithFile = set_path(FSBefore, ["testing", "123", "cheese"], {file, contents, new_file_info("cheese", file, 0)}),
%%     ModStateBefore = #msrv_state{current_dir = [[], "testing"], fs=FSBefore},
%%     ModStateAfter = #msrv_state{current_dir = [[], "testing"], fs=FSAfter},
%%     ModStateWithFile = #msrv_state{current_dir = [[], "testing"], fs=FSWithFile},
%%     StateBefore = #connection_state{module_state=ModStateBefore},
%%     StateAfter = #connection_state{module_state=ModStateAfter},
%%     StateWithFile = #connection_state{module_state=ModStateWithFile},
%%     {ok, StateAfter} = remove_directory(StateBefore, "123"),
%%     {ok, StateAfter} = remove_directory(StateBefore, "/testing/123"),
%%     {error, not_found} = remove_directory(StateBefore, "monkey"),
%%     {error, not_directory} = remove_directory(StateWithFile, "/testing/123/cheese"),
%%     {error, not_empty} = remove_directory(StateWithFile, "/testing/123").
%% 
%% -endif.

