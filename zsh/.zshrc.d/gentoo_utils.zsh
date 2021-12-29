function _ensure_gentoo_1 {
    [[ -e /etc/os-release ]] || return 1
    source /etc/os-release
    [[ $NAME -eq "Gentoo" ]] || return 1
    return 0
}

function _ensure_gentoo {
    _ensure_gentoo_1 || { echo "You're not on Gentoo!"; return }
}

function set_flag {
    local package=${1?"Usage: set_flag <package> <flag>"}
    local flag=${2?"Usage: set_flag <package> <flag>"}

    local current_state="default" desired_state
    case $flag in
        -*) desired_state="unset"
            flag=${flag#-}
            ;;
        %*) desired_state="default"
            flag=${flag#%}
            ;;
        *) desired_state="set";;
    esac

    if [[ -e /etc/portage/package.use/"$package" ]] && egrep -q "^$package.+[^-]$flag" /etc/portage/package.use/"$package"; then
        current_state="set"
    fi

    if [[ -e /etc/portage/package.use/"$package" ]] && egrep -q "^$package.+-$flag" /etc/portage/package.use/"$package"; then
        current_state="unset"
    fi

    if [[ $desired_state != $current_state ]]; then
        echo "Updating: $package $flag ($current_state -> $desired_state)"

        sudo mkdir -pv $(dirname /etc/portage/package.use/"$package")
        sudo touch /etc/portage/package.use/"$package"
        egrep -q "^$package" /etc/portage/package.use/"$package" || echo "$package" | sudo tee -a /etc/portage/package.use/"$package" >/dev/null

        sudo sed -i -E "s|\s-?$flag||g" /etc/portage/package.use/"$package"
        case $desired_state in
            "set") sudo sed -i -E "s|$package|& $flag|" /etc/portage/package.use/"$package";;
            "unset") sudo sed -i -E "s|$package|& -$flag|" /etc/portage/package.use/"$package";;
        esac
    fi

    grep --color=never "$package" /etc/portage/package.use/"$package"
}
