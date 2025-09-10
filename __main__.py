import re
import sys
import subprocess
from pathlib import Path

def convert_prolog_to_md(input_file: Path, output_file: Path):
    in_block_comment = False
    in_code_block = False
    first_line_done = False

    lines = input_file.read_text(encoding="utf-8").splitlines(keepends=True)
    out_lines = []

    for idx, line in enumerate(lines):
        stripped = line.strip()

        # Special handling for very first line: % filename.pl
        if not first_line_done:
            first_line_done = True
            m = re.match(r"%\s*(.+\.pl)", stripped)
            if m:
                out_lines.append(f"# `{m.group(1)}`\n\n")
                continue  # skip adding this line to code

        # Handle start of block comment /* ... */
        if "/*" in stripped and not in_block_comment:
            if in_code_block:
                out_lines.append("```\n")
                in_code_block = False
            in_block_comment = True
            comment_text = line.split("/*", 1)[1]
            if "*/" in comment_text:
                comment_text, _ = comment_text.split("*/", 1)
                in_block_comment = False
            out_lines.append(comment_text.strip() + "\n")
            continue

        # Handle inside block comment
        if in_block_comment:
            if "*/" in stripped:
                comment_text, _ = line.split("*/", 1)
                out_lines.append(comment_text.strip() + "\n")
                in_block_comment = False
            else:
                out_lines.append(line.strip() + "\n")
            continue

        # Handle Prolog code (including % line comments after first line)
        if not in_code_block:
            out_lines.append("```prolog\n")
            in_code_block = True

        out_lines.append(line)

    if in_code_block:
        out_lines.append("```\n")

    output_file.write_text("".join(out_lines), encoding="utf-8")


def main():
    source_dir = Path(".")
    build_dir = source_dir / ".build" / "__main__"
    build_dir.mkdir(parents=True, exist_ok=True)

    pl_files = list(source_dir.glob("*.pl"))

    if not pl_files:
        print("No .pl files found in current directory.")
        return

    for pl_file in pl_files:
        md_file = build_dir / (pl_file.stem + ".md")
        pdf_file = build_dir / (pl_file.stem + ".pdf")

        print(f"Converting {pl_file} -> {md_file}")
        convert_prolog_to_md(pl_file, md_file)

        print(f"Generating PDF {pdf_file}")
        subprocess.run(
            ["pandoc", str(md_file), "-o", str(pdf_file)],
            check=True
        )

if __name__ == "__main__":
    main()
