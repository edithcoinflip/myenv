# Defined in - @ line 1
function tw-make --description 'alias tw-make=docker run --rm -i -t -v (pwd):/source -w /source/ tastyworks/dev make -j4'
	docker run --rm -i -t -v (pwd):/source -w /source/ tastyworks/dev make -j4 $argv;
end
