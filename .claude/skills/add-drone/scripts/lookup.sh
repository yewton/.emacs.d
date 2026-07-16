#!/usr/bin/env bash
# Epkg データベース（emacsmirror のパッケージ定義）から、追加候補パッケージの
# 正規リポジトリ URL・概要・依存を引く。Web 検索で同名パッケージや fork を
# 誤って掴む事故を防ぐための、assimilate 前の確認手段。
# データベースは読み取り専用で開く（更新は update-epkgs.sh で行う）。
set -euo pipefail

EMACS_D="${EMACS_D:-$HOME/.emacs.d}"
DB="$EMACS_D/var/epkgs/epkg.db"

usage() {
  cat >&2 <<'EOF'
Usage: lookup.sh <name>

Epkg データベースから <name> のパッケージ定義を引き、class（github 等）・
正規 URL・summary・必須依存 feature を表示する。依存 feature には drone
登録状況（drone 登録済み / 別 drone が提供 / Emacs 組み込み / 未登録）を
付記する。

見つからない場合は名前の部分一致候補を提示する。データベース自体が古い
可能性がある場合は update-epkgs.sh（epkg-update を Emacs 経由で実行）で
更新してから再確認する。
EOF
  exit 1
}

[[ $# -eq 1 ]] || usage
name="$1"
if ! [[ "$name" =~ ^[A-Za-z0-9@+_.-]+$ ]]; then
  echo "Error: パッケージ名に使えない文字が含まれています: ${name}" >&2
  exit 1
fi
if [[ ! -f "$DB" ]]; then
  echo "Error: Epkg データベースが見つかりません: $DB" >&2
  exit 1
fi

q() { sqlite3 -readonly "$DB" "$1"; }

db_updated_at="$(git -C "$EMACS_D/var/epkgs" log -1 --format=%ad --date=short 2>/dev/null || echo 不明)"

row="$(q "select class, trim(url, '\"'), trim(summary, '\"') from packages where name = '\"${name}\"'")"
if [[ -z "$row" ]]; then
  echo "Epkg データベース（最終更新: ${db_updated_at}）に ${name} は見つかりませんでした。"
  candidates="$(q "select trim(name, '\"') || '  (' || class || ') ' || trim(summary, '\"') from packages where name like '%${name}%' limit 10")"
  if [[ -n "$candidates" ]]; then
    echo
    echo "名前の部分一致候補:"
    sed 's/^/  /' <<<"$candidates"
  fi
  echo
  echo "データベースが古いだけの可能性もある。update-epkgs.sh で更新して再確認すること。"
  echo "それでも見つからない場合のみ、upstream リポジトリを特定して URL をユーザーに確認する。"
  exit 1
fi

IFS='|' read -r class url summary <<<"$row"
echo "=== ${name}（Epkg データベース、最終更新: ${db_updated_at}）==="
echo "class:   ${class}"
echo "url:     ${url}"
echo "summary: ${summary}"
case "$class" in
  wiki)
    echo "注意: Emacswiki 由来のパッケージ。誰でも改変できる供給元なので、assimilate 前にユーザーに確認すること" ;;
  orphaned)
    echo "注意: Emacsorphanage 由来（メンテナ不在）のパッケージ。assimilate 前にユーザーに確認すること" ;;
esac

echo
echo "## 必須依存 (hard requires)"
feats="$(q "select feature from required where package = '\"${name}\"' and hard = 't' order by feature")"
if [[ -z "$feats" ]]; then
  echo "（なし）"
  exit 0
fi
while read -r feat; do
  [[ -n "$feat" ]] || continue
  if git config -f "$EMACS_D/.gitmodules" --get "submodule.${feat}.path" >/dev/null 2>&1; then
    echo "  ${feat}: drone 登録済み"
    continue
  fi
  provider_el="$(find "$EMACS_D/lib" -maxdepth 4 -name "${feat}.el" -print -quit 2>/dev/null)"
  if [[ -n "$provider_el" ]]; then
    rel="${provider_el#"$EMACS_D"/lib/}"
    echo "  ${feat}: drone ${rel%%/*} が提供"
    continue
  fi
  if emacs -Q --batch --eval "(kill-emacs (if (locate-library \"${feat}\") 0 1))" 2>/dev/null; then
    echo "  ${feat}: Emacs 組み込み"
  else
    echo "  ${feat}: ✗ 未登録 — この drone も追加が必要"
  fi
done <<<"$feats"
