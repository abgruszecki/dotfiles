import json
import sys
import subprocess

from kitty.boss import Boss
from kittens.tui.handler import kitten_ui

@kitten_ui(allow_remote_control=True)
def main(args: list[str]) -> str:
    # get the result of running kitten @ ls
    res = main.remote_control(['ls'], capture_output=True)
    if res.returncode != 0:
        sys.stderr.buffer.write(res.stderr)
        raise SystemExit(res.returncode)

    in_ = ''
    for r in json.loads(res.stdout):
        in_ += json.dumps(r)
    in_ += '\n'

    subprocess.run(
        ['vd', '-f', 'jsonl'],
        input=in_,
        text=True,
    )

    # # this is the main entry point of the kitten, it will be executed in
    # # the overlay window when the kitten is launched
    # answer = input('Enter some text: ')
    # # whatever this function returns will be available in the
    # # handle_result() function
    # return answer

def handle_result(args: list[str], answer: str, target_window_id: int, boss: Boss) -> None:
    # # get the kitty window into which to paste answer
    # w = boss.window_id_map.get(target_window_id)
    # if w is not None:
    #     w.paste_text(answer)
    pass
