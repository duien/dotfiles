function fish_title
	if [ $_ = 'fish' ]
        echo (pwd)
    else
        echo $_
    end
end
