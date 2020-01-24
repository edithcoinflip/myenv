# Defined in - @ line 1
function cgrab --description 'alias cgrab=grab && git gc --aggressive'
	grab && git gc --aggressive $argv;
end
