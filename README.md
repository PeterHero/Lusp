# Lusp (lua LSP)

Lusp is a simple lsp server implementation that currently supports basic LSP capabilities of textDocument sync.
There is no actual language parser beyond the JSON. That is possible future work to make this more useful. But it can be used as a framework for Haskell LSP for any language of choice.

## How to run

To run the example input on linux: `cat resources/example.txt | lusp`
Compiled using Haskell compiler, I use ghci inside cabal.

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


## Supported methods

### Initialize

The client calls this method so that the server can do setup. Server responses with available capabilites. Currently this server has only document sync capabilities.

Example:

```
Content-Length: 58

{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}
```

### Initialized

After receiving response to `initialize` method client sends `initialized` method, which is notification to indicate that the setup has completed successfully.
It expects no response.

Example:
```
Content-Length: 52

{"jsonrpc":"2.0","method":"initialized","params":{}}
```

### Shutdown

When client exits, it signalized `shutdown` to the server so that it can free up resources and/or exit.

Example:
```
Content-Length: 56

{"jsonrpc":"2.0","id":2,"method":"shutdown","params":{}}
```

### textDocument/didOpen

Part of document sync capability, client sends server a file and its contents that it has opened. It is a notification without response.

Example:
```
Content-Length: 154

{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///test.txt","languageId":"plaintext","version":1,"text":"Hello"}}}
```

### textDocument/didChange

Part of document sync capability, client send server a file which was updated and its contents. Again no response.

Example:
```
Content-Length: 158

{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///test.txt","version":2},"contentChanges":[{"text":"Hello world"}]}}
```

### textDocument/didClose

Part of document sync capability, client send server a file which it has closed. No response.

Example:
```
Content-Length: 103

{"jsonrpc":"2.0","method":"textDocument/didClose","params":{"textDocument":{"uri":"file:///test.txt"}}}
```

### Other

No other methods are supported. This is like a framework for Haskell LSP server, which can be used with parser for any language.

## Setup

For curious, this is how I integrated the LSP in my neovim config:
```
-- MY LUA LSP
vim.filetype.add({
  extension = {
    mylua = "mylua",
  }
})

vim.lsp.config('lusp', {
  cmd = { 'lusp' },
  filetypes = { 'mylua' },
  root_dir = vim.fs.dirname(vim.api.nvim_buf_get_name(0))
})

vim.lsp.enable('lusp')
```

## Debugging

For debugging I used this trick:

`socat -v TCP-LISTEN:9999,fork EXEC:"lusp`

and set neovim to connect:

`cmd = { 'nc', '127.0.0.1', '9999' },`
