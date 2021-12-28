function _p9k_taskwarrior_check_meta() {
  [[ -n $_p9k_taskwarrior_meta_sig ]] || return
  [[ -z $^_p9k_taskwarrior_meta_non_files(#qN) ]] || return
  local -a stat
  if (( $#_p9k_taskwarrior_meta_files )); then
    zstat -A stat +mtime -- $_p9k_taskwarrior_meta_files 2>/dev/null || return
  fi
  [[ $_p9k_taskwarrior_meta_sig == ${(pj:\0:)stat}$'\0'$TASKRC$'\0'$TASKDATA ]] || return
}


function _p9k_taskwarrior_init_meta() {
  local last_sig=$_p9k_taskwarrior_meta_sig
  {
    local cfg
    cfg="$(command task show data.location rc.color=0 rc._forcecolor=0 </dev/null 2>/dev/null)" || return
    local lines=(${(@M)${(f)cfg}:#data.location[[:space:]]##[^[:space:]]*})
    (( $#lines == 1 )) || return
    local dir=${lines[1]##data.location[[:space:]]#}
    : ${dir::=$~dir}  # `task` can give us path with `~`` in it; expand it

    local -a stat files=(${TASKRC:-~/.taskrc})
    _p9k_taskwarrior_meta_files=($^files(N))
    _p9k_taskwarrior_meta_non_files=(${files:|_p9k_taskwarrior_meta_files})
    if (( $#_p9k_taskwarrior_meta_files )); then
      zstat -A stat +mtime -- $_p9k_taskwarrior_meta_files 2>/dev/null || stat=(-1)
    fi
    _p9k_taskwarrior_meta_sig=${(pj:\0:)stat}$'\0'$TASKRC$'\0'$TASKDATA
    _p9k_taskwarrior_data_dir=$dir
  } always {
    if (( $? == 0 )); then
      _p9k__state_dump_scheduled=1
      return
    fi
    [[ -n $last_sig ]] && _p9k__state_dump_scheduled=1
    _p9k_taskwarrior_meta_files=()
    _p9k_taskwarrior_meta_non_files=()
    _p9k_taskwarrior_meta_sig=
    _p9k_taskwarrior_data_dir=
    _p9k__taskwarrior_functional=
  }
}


function _p9k_taskwarrior_check_data() {
  [[ -n $_p9k_taskwarrior_data_sig ]] || return
  [[ -z $^_p9k_taskwarrior_data_non_files(#qN) ]] || return
  local -a stat
  if (( $#_p9k_taskwarrior_data_files )); then
    zstat -A stat +mtime -- $_p9k_taskwarrior_data_files 2>/dev/null || return
  fi
  [[ $_p9k_taskwarrior_data_sig == ${(pj:\0:)stat}$'\0'$TASKRC$'\0'$TASKDATA ]] || return
  (( _p9k_taskwarrior_next_due == 0 || _p9k_taskwarrior_next_due > EPOCHSECONDS )) || return
}


function _p9k_taskwarrior_init_data() {
  local -a stat files=($_p9k_taskwarrior_data_dir/{pending,completed}.data)
  _p9k_taskwarrior_data_files=($^files(N))
  _p9k_taskwarrior_data_non_files=(${files:|_p9k_taskwarrior_data_files})
  if (( $#_p9k_taskwarrior_data_files )); then
    zstat -A stat +mtime -- $_p9k_taskwarrior_data_files 2>/dev/null || stat=(-1)
    _p9k_taskwarrior_data_sig=${(pj:\0:)stat}$'\0'
  else
    _p9k_taskwarrior_data_sig=
  fi

  _p9k_taskwarrior_data_files+=($_p9k_taskwarrior_meta_files)
  _p9k_taskwarrior_data_non_files+=($_p9k_taskwarrior_meta_non_files)
  _p9k_taskwarrior_data_sig+=$_p9k_taskwarrior_meta_sig

  local name val
  val="$(command task +PENDING count rc.color=0 rc._forcecolor=0 </dev/null 2>/dev/null)" || continue
  [[ $val == <1-> ]] && _p9k_taskwarrior_counters[PENDING]=$val

  val="$(command task +OVERDUE count rc.color=0 rc._forcecolor=0 </dev/null 2>/dev/null)" || continue
  [[ $val == <1-> ]] && _p9k_taskwarrior_counters[OVERDUE]=$val

  val="$(command task +PENDING count pri:L rc.color=0 rc._forcecolor=0 </dev/null 2>/dev/null)" || continue
  [[ $val == <1-> ]] && _p9k_taskwarrior_counters[LOW_PRIORITY]=$val

  val="$(command task +PENDING count pri:M rc.color=0 rc._forcecolor=0 </dev/null 2>/dev/null)" || continue
  [[ $val == <1-> ]] && _p9k_taskwarrior_counters[MED_PRIORITY]=$val

  val="$(command task +PENDING count pri:H rc.color=0 rc._forcecolor=0 </dev/null 2>/dev/null)" || continue
  [[ $val == <1-> ]] && _p9k_taskwarrior_counters[HIGH_PRIORITY]=$val

  
  _p9k_taskwarrior_next_due=0

  if (( _p9k_taskwarrior_counters[PENDING] > _p9k_taskwarrior_counters[OVERDUE] )); then
    local -a ts
    ts=($(command task +PENDING -OVERDUE list rc.verbose=nothing rc.color=0 rc._forcecolor=0 \
      rc.report.list.labels= rc.report.list.columns=due.epoch </dev/null 2>/dev/null)) || ts=()
    if (( $#ts )); then
      _p9k_taskwarrior_next_due=${${(on)ts}[1]}
      (( _p9k_taskwarrior_next_due > EPOCHSECONDS )) || _p9k_taskwarrior_next_due=$((EPOCHSECONDS+60))
    fi
  fi

  _p9k__state_dump_scheduled=1
}

function prompt_taskwarrior() {
  unset P9K_TASKWARRIOR_PENDING_COUNT P9K_TASKWARRIOR_OVERDUE_COUNT
  if ! _p9k_taskwarrior_check_data; then
    _p9k_taskwarrior_data_files=()
    _p9k_taskwarrior_data_non_files=()
    _p9k_taskwarrior_data_sig=
    _p9k_taskwarrior_counters=()
    _p9k_taskwarrior_next_due=0
    _p9k_taskwarrior_check_meta || _p9k_taskwarrior_init_meta || return
    _p9k_taskwarrior_init_data
  fi
  (( $#_p9k_taskwarrior_counters )) || return
  local text c=$_p9k_taskwarrior_counters[OVERDUE] found=""
  if [[ -n $c ]]; then
    typeset -g P9K_TASKWARRIOR_OVERDUE_COUNT=$c
    text+="$c 📅"
  fi

  if [[ -z $found ]]; then
      c=$_p9k_taskwarrior_counters[HIGH_PRIORITY]
      if [[ -n $c ]]; then
          typeset -g P9K_TASKWARRIOR_PENDING_COUNT=$c
          [[ -n $text ]] && text+='/'
          text+="$c ⇈"
          found="1"
      fi
  fi

  if [[ -z $found ]]; then
    c=$_p9k_taskwarrior_counters[MED_PRIORITY]
    if [[ -n $c ]]; then
        typeset -g P9K_TASKWARRIOR_PENDING_COUNT=$c
        [[ -n $text ]] && text+='/'
        text+="$c ●"
        found="1"
    fi
  fi

  if [[ -z $found ]]; then
      c=$_p9k_taskwarrior_counters[LOW_PRIORITY]
      if [[ -n $c ]]; then
          typeset -g P9K_TASKWARRIOR_PENDING_COUNT=$c
          [[ -n $text ]] && text+='/'
          text+="⇣"
          found="1"
      fi
  fi

  [[ -n $text ]] || return

  _p9k_prompt_segment $0 6 $_p9k_color1 TASKWARRIOR_ICON 0 '' "$text"
}


function _p9k_prompt_taskwarrior_init() {
  typeset -g "_p9k__segment_cond_${_p9k__prompt_side}[_p9k__segment_index]"='${commands[task]:+$_p9k__taskwarrior_functional}'
}

