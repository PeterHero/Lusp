# Lusp (lua LSP)

Lusp is a simple lua lsp server implementation the supports a subset of lua language and LSP features.

## Restrictions

TODO

## Interaction

Program communicates using LSP protocol. See [Full specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/).
LSP protocol defines header and content in JSON-RPC.
The program communicates using stdin and stdout.

Example:

```
Content-Length: ...\r\n
\r\n
{
	"jsonrpc": "2.0",
	"id": 1,
	"method": "textDocument/completion",
	"params": {
		...
	}
}
```
