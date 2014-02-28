function fish_title
	if [ $_ = 'fish' ]
        for part in (pwd_home)
          echo -n "$part/"
        end
    else
        echo $_
    end
end
