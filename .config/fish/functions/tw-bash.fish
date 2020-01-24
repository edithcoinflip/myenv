# Defined in - @ line 1
function tw-bash --description 'alias tw-bash=docker run --rm -i -t --env-file env.list --privileged -v (pwd):/source -w /source/ tastyworks/dev /bin/bash'
	docker run --rm -i -t --env-file env.list --privileged -v (pwd):/source -w /source/ tastyworks/dev /bin/bash $argv;
end
