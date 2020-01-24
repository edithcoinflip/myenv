# Defined in - @ line 1
function grab --description 'alias grab=git fetch -p && git fetch -t'
	git fetch -p && git fetch -t $argv;
end
