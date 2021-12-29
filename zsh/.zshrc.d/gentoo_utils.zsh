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
    local package=${1?"Usage: set_flag <package> <flag> [for-package]"}
    local flag=${2?"Usage: set_flag <package> <flag> [for-package]"}
    local for_package=${3:-$package}
    local namespace=${4:-use}

    local file=/etc/portage/package."$namespace"/"$for_package"

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

    if [[ -e "$file" ]] && egrep -q "^$package.+[^-]$flag" "$file"; then
        current_state="set"
    fi

    if [[ -e "$file" ]] && egrep -q "^$package.+-$flag" "$file"; then
        current_state="unset"
    fi

    if [[ $desired_state != $current_state ]]; then
        echo "Updating: $package $flag ($current_state -> $desired_state)"

        sudo mkdir -pv $(dirname "$file")
        sudo touch "$file"
        egrep -q "^$package" "$file" || echo "$package" | sudo tee -a "$file" >/dev/null

        sudo sed -i -E "s|\s-?$flag||g" "$file"
        case $desired_state in
            "set") sudo sed -i -E "s|$package|& $flag|" "$file";;
            "unset") sudo sed -i -E "s|$package|& -$flag|" "$file";;
        esac
    fi

    grep --color=never "$package" "$file"
}

function set_keyword {
    set_flag "$1" "$2" "$3" accept_keywords
}