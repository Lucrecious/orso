#ifndef GARBAGE_COLLECTOR_H_
#define GARBAGE_COLLECTOR_H_

#include "def.h"

// This is an Orso-oriented version of this GC: https://github.com/bullno1/ugc
// This has been rewritten from scratch but it takes major inspiration from its
// zero-memory allocation solution, double linked list and white marker flipping.
// It is still very close to original.

// Here's the license notice:
// Copyright (c) 2017, Bach Le
// All rights reserved.
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

typedef struct OrsoVM OrsoVM;

typedef struct OrsoGCHeader {
    struct OrsoGCHeader* previous;
    struct OrsoGCHeader* next;
    u32 color : 2;
} OrsoGCHeader;

typedef enum OrsoGCState {
    ORSO_GC_STATE_IDLE,
    ORSO_GC_STATE_MARKING,
    ORSO_GC_STATE_SWEEPING,
} OrsoGCState;

typedef struct OrsoGarbageCollector {
    OrsoGCHeader list1;
    OrsoGCHeader list2;

    OrsoGCHeader* from;
    OrsoGCHeader* to;
    OrsoGCHeader* iterator;

    OrsoGCState state;
    byte white;
    OrsoVM* vm;
} OrsoGarbageCollector;

void orso_gc_init(OrsoGarbageCollector* gc, OrsoVM* vm);

void orso_gc_register(OrsoGarbageCollector* gc, OrsoGCHeader* object);

void orso_gc_step(OrsoGarbageCollector* gc);

void orso_gc_collect(OrsoGarbageCollector* gc);


#endif
