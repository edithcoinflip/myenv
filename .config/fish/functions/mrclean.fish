# Defined in - @ line 1
function mrclean --description 'alias mrclean=git clean -X -d -f'
	git clean -X -d -f $argv;
end
