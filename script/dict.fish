# dict / dictiloray — Fish 补全
# 安装：ln -sf (pwd)/script/dict.fish ~/.config/fish/completions/dict.fish
# 若可执行文件名为 dict.scm，可复制本文件为 dict.scm.fish 或保留下方循环中的名称。

set -l __dict_cmds dict dict.scm

for c in $__dict_cmds
    complete -c $c -f

    complete -c $c -s h -l help -d '显示帮助'
    complete -c $c -l sentence -d '强制按句子处理'
    complete -c $c -l rare -d '句子模式：生僻词 + 金山释义（需 DEEPSEEK_API_KEY）'
    complete -c $c -l json -d '输出 JSON（单行 + 换行）'
    complete -c $c -s v -l verbose -d '更多候选；文本模式列出建议并显示英文例句'
    complete -c $c -l color -d '强制 ANSI 颜色'
    complete -c $c -l no-color -d '禁用颜色（亦遵守 NO_COLOR）'
    complete -c $c -l no-cache -d '不读缓存（仍会写入）'
    complete -c $c -l refresh -d '强制联网并覆盖缓存'
    complete -c $c -l clear-cache -d '清空缓存（不重置 lookup_stats）'
    complete -c $c -l no-count-beside -d '查阅次数仅在文末显示，不在词条标题旁'
    complete -c $c -l cache-db -d 'SQLite 缓存数据库路径' -r
    complete -c $c -l top -d '列出查词次数最高的 N 个词（仅统计）' -r

    # --cache-db 下一参数：路径
    complete -c $c -n '__fish_prev_arg_in --cache-db' -a '(__fish_complete_path)'

    # --top 下一参数：正整数（常见范围，可继续手输）
    complete -c $c -n '__fish_prev_arg_in --top' -a '(seq 1 50)'

    # --cache-db=… 当前 token 为等号形式时补全路径
    complete -c $c -n 'string match -q -- "--cache-db=*" (commandline -ct)' -a '(__fish_complete_path (string replace -r "^--cache-db=" "" (commandline -ct)))'

    # --top=… 当前 token
    complete -c $c -n 'string match -q -- "--top=*" (commandline -ct)' -a '(seq 1 50)'
end
