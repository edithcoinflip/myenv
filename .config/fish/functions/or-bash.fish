# Defined in - @ line 1
function or-bash --description 'alias or-bash=docker run --rm -i -t --privileged -v (pwd):/source -w /source/Servers/sxOR2/ tastyworks/dev /bin/bash'
	docker run --rm -i -t --privileged -v (pwd):/source -w /source/Servers/sxOR2/ tastyworks/dev /bin/bash $argv;
end
