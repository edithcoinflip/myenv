# Defined in - @ line 1
function or-make --description 'alias or-make=docker run --rm -i -t -v (pwd):/source -w /source/Servers/sxOR2/ tastyworks/dev make -j4'
	docker run --rm -i -t -v (pwd):/source -w /source/Servers/sxOR2/ tastyworks/dev make -j4 $argv;
end
