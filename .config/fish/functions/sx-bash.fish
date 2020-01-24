# Defined in - @ line 1
function sx-bash --description 'alias sx-bash=docker run --rm -i -t --privileged -v (pwd):/source -w /source/ tastyworks/dev /bin/bash'
	docker run --rm -i -t --privileged -v (pwd):/source -w /source/ tastyworks/dev /bin/bash $argv;
end
