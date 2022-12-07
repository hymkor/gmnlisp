package command

import (
	"context"
	"os"
	"os/exec"

	"github.com/hymkor/gmnlisp"
)

func init() {
	gmnlisp.AutoLoad(gmnlisp.NewSymbol("command"), &gmnlisp.Function{F: funCommand})
}

func funCommand(ctx context.Context, w *gmnlisp.World, list []gmnlisp.Node) (gmnlisp.Node, error) {
	argv := make([]string, len(list))
	for i, value := range list {
		argv[i] = value.String()
	}

	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdout = w.Stdout()
	cmd.Stderr = w.Errout()
	cmd.Stdin = os.Stdin // w.Stdin()
	return gmnlisp.Null, cmd.Run()
}
