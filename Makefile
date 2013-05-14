PREFIX:=../
DAY=`date +%Y%m%d%H%M`
DEST:=$(PREFIX)$(PROJECT)
ERL=erl
REBAR=./rebar


rebuild:	del_deps \
	get_deps \
	clean \
	compile

test: clean \
	compile
	@$(REBAR) ct

clean:
	@$(REBAR) clean
	@rm -rf ./test/*.beam

compile:
	@$(REBAR) compile

get_deps:	del_deps
	@$(REBAR) get-deps

del_deps:
	@rm -rf ./deps
	
release:
	rm -rf erlang-ftpserver;
	rm -rf erlang-ftpserver;
	mkdir ./erlang-ftpserver
	cp -r ./src/ ./erlang-ftpserver/ 
	cp -r ./deps/ ./erlang-ftpserver/ 
	cp -r ./include/ ./erlang-ftpserver/ 
	cp ./rebar ./erlang-ftpserver/rebar
	cp ./rebar.config.release ./erlang-ftpserver/rebar.config
	cd ./erlang-ftpserver;cp ./src/ftpserver.app.release ./src/ftpserver.app.src;./rebar clean;./rebar compile;mkdir ./ftpserver;mv ./ebin ./ftpserver;mkdir ./rel;cd ./rel;../rebar create-node nodeid=erlang-ftpserver;cp ../../reltool.config ./;cp ../../sys.config ./files/sys.config;
	cd ./erlang-ftpserver;./rebar generate
	cp -r ./erlang-ftpserver/rel/erlang-ftpserver ./
	zip -r erlang-ftpserver-$(DAY) ./erlang-ftpserver
	rm -rf erlang-ftpserver;rm -rf erlang-ftpserver

	
