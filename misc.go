package gmnlisp

import (
	"context"
	"time"
)

func funUniversalTime(ctx context.Context, w *World) (Node, error) {
	start := time.Date(1900, time.Month(1), 1, 0, 0, 0, 0, time.UTC)
	now := time.Now()
	duration := now.Sub(start)

	return Integer(duration / time.Second), nil
}

func funInternalRealTime(ctx context.Context, w *World) (Node, error) {
	start := time.Date(2000, time.Month(1), 1, 0, 0, 0, 0, time.UTC)
	now := time.Now()

	return Integer(now.Sub(start)), nil
}

func funInternalTimeUnitPerSecond(ctx context.Context, w *World) (Node, error) {
	return Integer(time.Second), nil
}
