import builtins
from pprint import pprint
from rich import print as rprint, inspect
from rich.console import Console
from rich.pretty import pprint as rpprint
from rich.traceback import install as install_rich_traceback

try:
    # Install rich tracebacks globally
    install_rich_traceback(
        show_locals=True,
        max_frames=10,
        width=100,
        word_wrap=True,
        extra_lines=3,
    )

    console = Console()

    # Override IPython's result display
    def rich_result_display(obj):
        console.print(obj)

    # Get IPython instance and set custom formatter
    ip = get_ipython()
    if ip:
        # Set the result display formatter
        ip.display_formatter.formatters["text/plain"].for_type(
            object, rich_result_display
        )

    # Convenient aliases
    builtins.pp = rpprint
    builtins.rprint = rprint
    builtins.inspect = inspect
    builtins.console = console

    def print_exc():
        console.print_exception(show_locals=True)

    builtins.print_exc = print_exc
except ImportError:
    # Fallback to standard library
    builtins.pp = pprint
    builtins.rprint = print

    def basic_inspect(obj):
        print(f"Type: {type(obj)}")
        if hasattr(obj, "__dict__"):
            pprint(vars(obj))
        else:
            print(repr(obj))

    builtins.inspect = basic_inspect

    import traceback

    def print_exc():
        traceback.print_exc()

    builtins.print_exc = print_exc
