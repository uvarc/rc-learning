---
title: ##################### PLOT MSA WITH COVERAGE ####################
date: 2025-05-20-00:23:54Z
type: docs 
weight: 4300
menu: 
    rivanna-alphafold:
---


plt.figure(figsize=(8, 4), dpi=100)

plt.title(f"Sequence coverage ({name})")

plt.imshow(final,

interpolation='nearest', aspect='auto',

cmap="rainbow_r", vmin=0, vmax=1, origin='lower')

plt.plot((msa != 21).sum(0), color='black')

plt.xlim(-0.5, msa.shape[1] - 0.5)

plt.ylim(-0.5, msa.shape[0] - 0.5)

plt.colorbar(label="Sequence identity to query", )

plt.xlabel("Positions")

plt.ylabel("Sequences")

plt.savefig(f"{out_dir}/{name}_msa_sequence_coverage.pdf")

generate_seq_cov_plot(args.name, args.input_dir, args.output_dir)

