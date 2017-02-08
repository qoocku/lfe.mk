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
	$(verbose) PATH=$(PATH):$(DEPS_DIR)/lfe/bin lfe -pa ebin -pa test

### Templates

# template render hook for LFE (extension must be .lfe)

override define render_template
	$(verbose) printf -- '$(subst $(newline),\n,$(subst %,%%,$(subst ','\'',$(subst $(tab),$(WS),$(call $(1))))))\n' > $(2)
	$(verbose) if [ "$(subst lfe-,,$(1))" != "$(1)" ]; then \
		mv $(2) $(basename $(2)).lfe ; \
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
	    `#(#(one_for_one 1 5) ,procs)))
endef

define tpl_lfe-gen-server
(defmodule $(n)
  (behaviour gen_server)
  ;; API.
  (export [start_link 0])

  ;; gen_server callbacks
  (export [init 1]
          [handle_call 3]
          [handle_cast 2]
          [handle_info 2]
          [terminate 2]
          [code_change 3]))

(defrecord state)

;;; API.

#|(defspec (start_link 0) ([] `#(ok ,(pid))))|#
(defun start_link ()
  (gen_server:start_link (MODULE) [] []))

;; gen_server callbacks

(defun init (_args)
  `#(ok ,(make-state)))

(defun handle_call (_request _from state)
  `#(reply ignored ,state))

(defun handle_cast (_msg state)
  `#(noreply ,state))

(defun handle_info (_info state)
  `#(noreply ,state))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-vsn state _extra)
  `#(ok ,state))
endef

define tpl_module
(defmodule $(n)
  "A module."
  (export []))
endef

define tpl_lfe-cowboy-http
(defmodule $(n)
  (behaviour cowboy_http_handler)

  (export [init 3]
          [handle 2]
          [terminate 3]))

(defrecord state)

(defun init (_ req _opts)
  `#(ok req ,(make-state)))

(defun handle
  [(req (= state (make-state)))
   (let (((tuple ok req2) (cowboy_req:reply 200  req)))
     `#(ok ,req2 ,state))])

(defun terminate (_reason _req _state)
  'ok)
endef

define tpl_lfe-gen-fsm
(defmodule $(n)
  "FSM callback module.
  "

  (behaviour gen_fsm)

  ;; API.
  (export [start_link 0])

  ;; gen_fsm callbacks
  (export [init 1]
          [state-name 2]
          [state-name 3]
          [handle_event 3]
          [handle_sync_event 4]
          [handle_info 3]
          [terminate 3]
          [code_change 4]))

(defrecord state)

;;; API.

#|(defspec start_link () `#(ok (pid)))|#
(defun start_link ()
  (gen_fsm:start_link (MODULE) [] []))

;;; gen_fsm callbacks

(defun init (_arg)
  `#(ok state_name ,(make-state)))

(defun state-name (_event state-data)
  `#(next_state state-name ,state-data))

(defun handle_event (_event state-name state-data)
  `#(next_state ,state-name ,state-data))

(defun state-name (_event _from, state-data)
  `#(reply ignored state-name ,state-data))

(defun handle_sync_event (_event _from state-name state-data)
  `#(reply ignored ,state-name ,state-data))

(defun handle_info (_info state-name state-data)
  `#(next_state ,state-name ,state-data))

(defun terminate (_reason _state-name _state-data)
  'ok)

(defun code_change (_old-vsn state-name state-data _extra)
  `#(ok ,state-name ,state-data))
endef

define tpl_lfe-cowboy-loop
(defmodule $(n)
  "Cowboy loop handler callback module.
  "
  (behaviour cowboy_loop_handler)
  (export [init 2]
          [info 3]
          [terminate 3]))

(defrecord state)

(defun init (req _opts)
  `#(loop ,req ,(make-state) 5000 hibernate))

(defun info (_info req state)
  `#(loop ,req ,state hibernate))

(defun terminate (_reason _req _state)
  'ok)
endef

define tpl_lfe-cowboy-rest
(defmodule $(n)
  "Cowboy REST callbacks."
  (export [init 3]
          [content_types_provided 2]
          [get_html 2]))

(defun init (_ _req _opts)
  #(upgrade protocol cowboy_rest))

(defun content_types_provided (req state)
  `#([#(#(<<"text">> <<"html">> '*) get_html)] ,req ,state))

(defun get_html (req state)
  `#(<<"<html><body>This is REST!</body></html>">> ,req ,state))
endef
