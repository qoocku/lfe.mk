# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

.PHONY: lfe-shell

# Verbosity.

lfe_verbose_0 = @echo " LFE   " $(filter %.lfe,$(?F));
lfe_verbose = $(lfe_verbose_$(V))

# Core targets.

LFE_FILES = $(sort $(call core_find,src/,*.lfe))

ifneq ($(LFE_FILES),)

BEAM_FILES += $(addprefix ebin/,$(patsubst %.lfe,%.beam,$(notdir $(LFE_FILES))))

# Rebuild LFE modules when the Makefile changes.
$(LFE_FILES): $(MAKEFILE_LIST)
	@touch $@

ebin/$(PROJECT).app:: $(LFE_FILES) | ebin/
	$(if $(strip $?),$(lfe_verbose) PATH=$(PATH):$(DEPS_DIR)/lfe/bin lfec -o ebin/ $(LFE_FILES))

endif

# Shell.

lfe-shell: deps
	$(verbose) PATH=$(PATH):$(DEPS_DIR)/lfe/bin lfe

### Templates

# template render hook for LFE (extension must be .lfe)
define render_template
	$(verbose) printf -- '$(subst $(newline),\n,$(subst %,%%,$(subst ','\'',$(subst $(tab),$(WS),$(call $(1))))))\n' > $(2)
	if [ "$(subst lfe-,,$(1))" != "$(1)" ]; then
		mv $(2) $(basename $(2)).lfe
	fi
endef

define tpl_lfe-supervisor
(defmodule $(n)
	(behaviour supervisor)
	(export [start_link 0]
		    [init 1]))

(defun start_link ()
	"Supervisor's launching."
	(supervisor:start_link `#(local ,(MODULE)) (MODULE) []))

(defun init (_args)
	"Initialisation of supervisor process."
	(let ((procs []))
	    `#(#(one_for_one 1 5) ,procs))))
endef
